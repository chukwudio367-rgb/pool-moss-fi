;; Pool Moss Finance Yield Optimization Protocol
;; Clarity Version 2 / Epoch 2.1

;; Features:
;;   - Liquidity pools with deposit/withdraw
;;   - Dynamic performance fees (reduced for loyal users)
;;   - Yield Symbiosis Vaults (compound yield tracking)
;;   - Position health monitoring
;;   - Protocol admin controls

;; ============================================================
;; Constants
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)

(define-constant ERR-NOT-AUTHORIZED     (err u100))
(define-constant ERR-POOL-NOT-FOUND     (err u101))
(define-constant ERR-VAULT-NOT-FOUND    (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-ZERO-AMOUNT        (err u104))
(define-constant ERR-POOL-INACTIVE      (err u105))
(define-constant ERR-UNHEALTHY-POSITION (err u106))
(define-constant ERR-ALREADY-EXISTS     (err u107))

;; Fee constants (basis points, 1 bp = 0.01%)
(define-constant BASE-PERFORMANCE-FEE u500)   ;; 5% base performance fee
(define-constant LOYAL-USER-FEE       u200)   ;; 2% for loyal users
(define-constant BRIDGE-FEE-BPS       u30)    ;; 0.3% bridge fee
;; Loyalty threshold: deposits needed for reduced fee
(define-constant LOYALTY-THRESHOLD    u10)

;; Health factor threshold (scaled by 1e6)
;; Positions below this are considered unhealthy
(define-constant MIN-HEALTH-FACTOR u800000)   ;; 0.8

;; ============================================================
;; Data Maps and Variables
;; ============================================================

;; Protocol-level stats
(define-data-var total-value-locked uint u0)
(define-data-var total-pools uint u0)
(define-data-var protocol-paused bool false)
(define-data-var treasury-balance uint u0)

;; Liquidity pool definition
;; pool-id => pool data
(define-map pools
  { pool-id: uint }
  {
    name: (string-ascii 64),
    total-liquidity: uint,
    total-shares: uint,
    active: bool,
    yield-rate-bps: uint,    ;; annualized yield in basis points
    last-rebalance: uint     ;; block height of last rebalance
  }
)

;; User shares per pool
;; (pool-id, user) => shares and deposit count
(define-map user-pool-positions
  { pool-id: uint, user: principal }
  {
    shares: uint,
    deposit-count: uint,     ;; tracks loyalty
    last-deposit-block: uint
  }
)

;; Yield Symbiosis Vaults
;; vault-id => vault data
(define-map vaults
  { vault-id: uint }
  {
    owner: principal,
    pool-id: uint,
    deposited: uint,
    compounded-yield: uint,
    health-factor: uint,     ;; scaled by 1e6
    created-at: uint
  }
)

(define-data-var total-vaults uint u0)

;; Cross-chain bridge fee accumulator (simulated)
(define-map bridge-fee-ledger
  { chain-id: uint }
  { accumulated-fees: uint }
)

;; ============================================================
;; Read-Only Functions
;; ============================================================

(define-read-only (get-pool (pool-id uint))
  (map-get? pools { pool-id: pool-id })
)

(define-read-only (get-user-position (pool-id uint) (user principal))
  (map-get? user-pool-positions { pool-id: pool-id, user: user })
)

(define-read-only (get-vault (vault-id uint))
  (map-get? vaults { vault-id: vault-id })
)

(define-read-only (get-total-value-locked)
  (var-get total-value-locked)
)

(define-read-only (get-treasury-balance)
  (var-get treasury-balance)
)

(define-read-only (get-total-pools)
  (var-get total-pools)
)

(define-read-only (get-total-vaults)
  (var-get total-vaults)
)

(define-read-only (is-protocol-paused)
  (var-get protocol-paused)
)

;; Calculate performance fee for a user based on loyalty
(define-read-only (get-user-fee-bps (pool-id uint) (user principal))
  (match (map-get? user-pool-positions { pool-id: pool-id, user: user })
    position
      (if (>= (get deposit-count position) LOYALTY-THRESHOLD)
        LOYAL-USER-FEE
        BASE-PERFORMANCE-FEE)
    BASE-PERFORMANCE-FEE
  )
)

;; Calculate shares to issue for a deposit
;; shares = (amount / total-liquidity) * total-shares
;; For first deposit, shares = amount
(define-read-only (calculate-shares (pool-id uint) (amount uint))
  (match (map-get? pools { pool-id: pool-id })
    pool
      (let (
        (liquidity (get total-liquidity pool))
        (shares    (get total-shares pool))
      )
        (if (or (is-eq liquidity u0) (is-eq shares u0))
          amount
          (/ (* amount shares) liquidity)
        )
      )
    amount
  )
)

;; Calculate amount redeemable for a given number of shares
(define-read-only (calculate-redeem-amount (pool-id uint) (user-shares uint))
  (match (map-get? pools { pool-id: pool-id })
    pool
      (let (
        (liquidity (get total-liquidity pool))
        (shares    (get total-shares pool))
      )
        (if (is-eq shares u0)
          u0
          (/ (* user-shares liquidity) shares)
        )
      )
    u0
  )
)

;; Health factor: simple ratio of vault value to deposited (scaled 1e6)
(define-read-only (compute-health-factor (deposited uint) (compounded uint))
  (if (is-eq deposited u0)
    u0
    (/ (* (+ deposited compounded) u1000000) deposited)
  )
)

;; ============================================================
;; Admin Functions
;; ============================================================

(define-public (set-protocol-paused (paused bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (var-set protocol-paused paused))
  )
)

;; Create a new liquidity pool
(define-public (create-pool (name (string-ascii 64)) (yield-rate-bps uint))
  (let ((pool-id (+ (var-get total-pools) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? pools { pool-id: pool-id })) ERR-ALREADY-EXISTS)
    (map-set pools
      { pool-id: pool-id }
      {
        name:             name,
        total-liquidity:  u0,
        total-shares:     u0,
        active:           true,
        yield-rate-bps:   yield-rate-bps,
        last-rebalance:   block-height
      }
    )
    (var-set total-pools pool-id)
    (ok pool-id)
  )
)

;; Set pool active/inactive
(define-public (set-pool-active (pool-id uint) (active bool))
  (let ((pool (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (map-set pools { pool-id: pool-id }
          (merge pool { active: active })))
  )
)

;; Update pool yield rate (simulates Moss Growth Algorithm rebalancing)
(define-public (rebalance-pool (pool-id uint) (new-yield-rate-bps uint))
  (let ((pool (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (map-set pools { pool-id: pool-id }
          (merge pool {
            yield-rate-bps:  new-yield-rate-bps,
            last-rebalance:  block-height
          })))
  )
)

;; Withdraw accumulated treasury fees
(define-public (withdraw-treasury (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= amount (var-get treasury-balance)) ERR-INSUFFICIENT-FUNDS)
    (var-set treasury-balance (- (var-get treasury-balance) amount))
    ;; In a real deployment this would call stx-transfer? to recipient
    (ok amount)
  )
)

;; ============================================================
;; User-Facing Pool Functions
;; ============================================================

;; Deposit STX into a pool and receive shares
;; NOTE: In production this integrates with stx-transfer? or a SIP-010 token
(define-public (deposit (pool-id uint) (amount uint))
  (let (
    (pool     (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND))
    (position (default-to
                { shares: u0, deposit-count: u0, last-deposit-block: u0 }
                (map-get? user-pool-positions { pool-id: pool-id, user: tx-sender })))
    (new-shares (calculate-shares pool-id amount))
  )
    (asserts! (not (var-get protocol-paused)) ERR-POOL-INACTIVE)
    (asserts! (get active pool) ERR-POOL-INACTIVE)
    (asserts! (> amount u0) ERR-ZERO-AMOUNT)

    ;; Update pool liquidity and shares
    (map-set pools { pool-id: pool-id }
      (merge pool {
        total-liquidity: (+ (get total-liquidity pool) amount),
        total-shares:    (+ (get total-shares pool) new-shares)
      })
    )

    ;; Update user position
    (map-set user-pool-positions
      { pool-id: pool-id, user: tx-sender }
      {
        shares:             (+ (get shares position) new-shares),
        deposit-count:      (+ (get deposit-count position) u1),
        last-deposit-block: block-height
      }
    )

    ;; Update global TVL
    (var-set total-value-locked (+ (var-get total-value-locked) amount))

    (ok new-shares)
  )
)

;; Withdraw from a pool by redeeming shares
(define-public (withdraw (pool-id uint) (shares-to-redeem uint))
  (let (
    (pool     (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND))
    (position (unwrap! (map-get? user-pool-positions { pool-id: pool-id, user: tx-sender })
                ERR-INSUFFICIENT-FUNDS))
    (user-shares (get shares position))
    (raw-amount  (calculate-redeem-amount pool-id shares-to-redeem))
    (fee-bps     (get-user-fee-bps pool-id tx-sender))
    ;; fee = raw-amount * fee-bps / 10000
    (fee-amount  (/ (* raw-amount fee-bps) u10000))
    (net-amount  (- raw-amount fee-amount))
  )
    (asserts! (not (var-get protocol-paused)) ERR-POOL-INACTIVE)
    (asserts! (get active pool) ERR-POOL-INACTIVE)
    (asserts! (> shares-to-redeem u0) ERR-ZERO-AMOUNT)
    (asserts! (>= user-shares shares-to-redeem) ERR-INSUFFICIENT-FUNDS)
    (asserts! (> raw-amount u0) ERR-INSUFFICIENT-FUNDS)

    ;; Accumulate fee in treasury
    (var-set treasury-balance (+ (var-get treasury-balance) fee-amount))

    ;; Update pool
    (map-set pools { pool-id: pool-id }
      (merge pool {
        total-liquidity: (- (get total-liquidity pool) raw-amount),
        total-shares:    (- (get total-shares pool) shares-to-redeem)
      })
    )

    ;; Update user position
    (map-set user-pool-positions
      { pool-id: pool-id, user: tx-sender }
      (merge position { shares: (- user-shares shares-to-redeem) })
    )

    ;; Update global TVL
    (var-set total-value-locked
      (if (>= (var-get total-value-locked) raw-amount)
        (- (var-get total-value-locked) raw-amount)
        u0
      )
    )

    ;; In production: stx-transfer? net-amount to tx-sender
    (ok { net-amount: net-amount, fee-paid: fee-amount })
  )
)

;; ============================================================
;; Yield Symbiosis Vaults
;; ============================================================

;; Open a new vault for a given pool
(define-public (open-vault (pool-id uint) (initial-deposit uint))
  (let (
    (pool     (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND))
    (vault-id (+ (var-get total-vaults) u1))
    (health   (compute-health-factor initial-deposit u0))
  )
    (asserts! (not (var-get protocol-paused)) ERR-POOL-INACTIVE)
    (asserts! (get active pool) ERR-POOL-INACTIVE)
    (asserts! (> initial-deposit u0) ERR-ZERO-AMOUNT)

    (map-set vaults { vault-id: vault-id }
      {
        owner:            tx-sender,
        pool-id:          pool-id,
        deposited:        initial-deposit,
        compounded-yield: u0,
        health-factor:    health,
        created-at:       block-height
      }
    )
    (var-set total-vaults vault-id)

    ;; Also register deposit in pool
    (try! (deposit pool-id initial-deposit))

    (ok vault-id)
  )
)

;; Compound yield into a vault (called by keeper or owner)
;; yield-amount represents earned yield being added to the vault
(define-public (compound-vault (vault-id uint) (yield-amount uint))
  (let (
    (vault (unwrap! (map-get? vaults { vault-id: vault-id }) ERR-VAULT-NOT-FOUND))
    (new-compounded (+ (get compounded-yield vault) yield-amount))
    (new-health     (compute-health-factor (get deposited vault) new-compounded))
  )
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER)
                  (is-eq tx-sender (get owner vault)))
              ERR-NOT-AUTHORIZED)
    (asserts! (> yield-amount u0) ERR-ZERO-AMOUNT)

    (map-set vaults { vault-id: vault-id }
      (merge vault {
        compounded-yield: new-compounded,
        health-factor:    new-health
      })
    )

    (ok new-health)
  )
)

;; Close a vault and return funds to owner
;; Enforces health factor check before allowing closure
(define-public (close-vault (vault-id uint))
  (let (
    (vault (unwrap! (map-get? vaults { vault-id: vault-id }) ERR-VAULT-NOT-FOUND))
    (total-value (+ (get deposited vault) (get compounded-yield vault)))
  )
    (asserts! (is-eq tx-sender (get owner vault)) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get health-factor vault) MIN-HEALTH-FACTOR) ERR-UNHEALTHY-POSITION)

    ;; Remove vault record
    (map-delete vaults { vault-id: vault-id })

    ;; In production: release funds back to owner via stx-transfer?
    (ok total-value)
  )
)

;; ============================================================
;; Bridge Fee Simulation
;; Cross-chain routing fees accumulate per chain ID
;; ============================================================

;; Record a cross-chain bridge operation and collect fee
;; bridge-amount: value being bridged
;; chain-id: destination chain identifier
(define-public (record-bridge-operation (bridge-amount uint) (destination-chain uint))
  (let (
    (fee-amount (/ (* bridge-amount BRIDGE-FEE-BPS) u10000))
    (ledger     (default-to { accumulated-fees: u0 }
                  (map-get? bridge-fee-ledger { chain-id: destination-chain })))
  )
    (asserts! (> bridge-amount u0) ERR-ZERO-AMOUNT)

    (map-set bridge-fee-ledger { chain-id: destination-chain }
      { accumulated-fees: (+ (get accumulated-fees ledger) fee-amount) }
    )
    (var-set treasury-balance (+ (var-get treasury-balance) fee-amount))

    (ok { fee-collected: fee-amount, destination-chain: destination-chain })
  )
)

(define-read-only (get-bridge-fees (chain-id uint))
  (default-to { accumulated-fees: u0 }
    (map-get? bridge-fee-ledger { chain-id: chain-id }))
)
