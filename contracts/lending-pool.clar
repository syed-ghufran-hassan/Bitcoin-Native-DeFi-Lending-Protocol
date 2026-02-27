;; ============================================================
;; PRODUCTION LENDING POOL (STX-ONLY COLLATERAL)
;; ============================================================

(use-trait liquidator-trait .liquidator-trait.liquidator-trait)

;; ============================================================
;; ERRORS
;; ============================================================

(define-constant ERR-OWNER-ONLY (err u400))
(define-constant ERR-INSUFFICIENT-BALANCE (err u401))
(define-constant ERR-INSUFFICIENT-COLLATERAL (err u402))
(define-constant ERR-LOAN-NOT-FOUND (err u403))
(define-constant ERR-POSITION-HEALTHY (err u404))
(define-constant ERR-INVALID-AMOUNT (err u405))
(define-constant ERR-PAUSED (err u406))
(define-constant ERR-NOT-VERIFIED (err u407))

;; ============================================================
;; PROTOCOL CONSTANTS
;; ============================================================

(define-constant COLLATERAL-RATIO u150)         ;; 150%
(define-constant MIN-HEALTH-FACTOR u120)        ;; 120%
(define-constant LIQUIDATION-BONUS u10)         ;; 10%
(define-constant INTEREST-RATE-BPS u500)        ;; 5%

(define-constant SECONDS-PER-YEAR u31536000)
(define-constant BPS-DENOMINATOR u10000)

(define-constant CONTRACT-ADDRESS .lending-pool)

;; ============================================================
;; STATE
;; ============================================================

(define-data-var admin principal tx-sender)
(define-data-var protocol-paused bool false)

(define-data-var total-deposits uint u0)
(define-data-var total-borrows uint u0)

(define-data-var verified-liquidator-hash (optional (buff 32)) none)

;; ============================================================
;; STORAGE
;; ============================================================

(define-map user-deposits
  { user: principal }
  { amount: uint }
)

(define-map user-collateral
  { user: principal }
  { amount: uint }
)

(define-map user-loans
  { user: principal }
  {
    principal-amount: uint,
    borrow-time: uint,
    last-interest-update: uint,
  }
)

;; ============================================================
;; INTERNAL HELPERS
;; ============================================================

(define-private (assert-not-paused)
  (asserts! (not (var-get protocol-paused)) ERR-PAUSED)
)

(define-private (get-available-liquidity)
  (- (var-get total-deposits) (var-get total-borrows))
)

;; ------------------------------------------------------------
;; Interest Calculation
;; ------------------------------------------------------------

(define-read-only (calculate-current-interest (user principal))
  (match (map-get? user-loans { user: user })
    loan
      (let (
        (elapsed (if (> stacks-block-time (get last-interest-update loan))
                    (- stacks-block-time (get last-interest-update loan))
                    u0))
      )
        (ok
          (/ (* (* (get principal-amount loan) INTEREST-RATE-BPS) elapsed)
             (* SECONDS-PER-YEAR BPS-DENOMINATOR)
          )
        )
      )
    (ok u0)
  )
)

(define-private (accrue-interest (user principal))
  (match (map-get? user-loans { user: user })
    loan
      (let (
        (interest (unwrap-panic (calculate-current-interest user)))
        (new-principal (+ (get principal-amount loan) interest))
      )
        (map-set user-loans { user: user } {
          principal-amount: new-principal,
          borrow-time: (get borrow-time loan),
          last-interest-update: stacks-block-time,
        })
        (var-set total-borrows (+ (var-get total-borrows) interest))
        interest
      )
    u0
  )
)

;; ------------------------------------------------------------
;; Health Factor
;; ------------------------------------------------------------

(define-private (calculate-health-factor (user principal))
  (match (map-get? user-loans { user: user })
    loan
      (match (map-get? user-collateral { user: user })
        coll
          (let (
            (interest (unwrap-panic (calculate-current-interest user)))
            (total-debt (+ (get principal-amount loan) interest))
          )
            (if (is-eq total-debt u0)
                u0
                (/ (* (get amount coll) u100) total-debt)
            )
          )
        u0
      )
    u0
  )
)

;; ============================================================
;; DEPOSIT / WITHDRAW
;; ============================================================

(define-public (deposit (amount uint))
  (begin
    (assert-not-paused)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    (try! (stx-transfer? amount tx-sender CONTRACT-ADDRESS))

    (let ((current (default-to { amount: u0 }
                    (map-get? user-deposits { user: tx-sender }))))
      (map-set user-deposits { user: tx-sender } {
        amount: (+ (get amount current) amount)
      })
    )

    (var-set total-deposits (+ (var-get total-deposits) amount))
    (ok true)
  )
)

(define-public (withdraw (amount uint))
  (let ((deposit (unwrap! (map-get? user-deposits { user: tx-sender })
                 ERR-INSUFFICIENT-BALANCE)))
    (assert-not-paused)
    (asserts! (>= (get amount deposit) amount)
              ERR-INSUFFICIENT-BALANCE)

    (map-set user-deposits { user: tx-sender } {
      amount: (- (get amount deposit) amount)
    })

    (try! (stx-transfer? amount CONTRACT-ADDRESS tx-sender))
    (var-set total-deposits (- (var-get total-deposits) amount))

    (ok true)
  )
)

;; ============================================================
;; COLLATERAL
;; ============================================================

(define-public (add-collateral (amount uint))
  (begin
    (assert-not-paused)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    (try! (stx-transfer? amount tx-sender CONTRACT-ADDRESS))

    (let ((current (default-to { amount: u0 }
                    (map-get? user-collateral { user: tx-sender }))))
      (map-set user-collateral { user: tx-sender } {
        amount: (+ (get amount current) amount)
      })
    )

    (ok true)
  )
)

;; ============================================================
;; BORROW / REPAY
;; ============================================================

(define-public (borrow (amount uint))
  (let (
        (coll (unwrap! (map-get? user-collateral { user: tx-sender })
               ERR-INSUFFICIENT-COLLATERAL))
      )
    (assert-not-paused)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    (accrue-interest tx-sender)

    (let (
          (loan (map-get? user-loans { user: tx-sender }))
          (existing (match loan l (get principal-amount l) u0))
          (new-debt (+ existing amount))
          (max-borrow (/ (* (get amount coll) u100) COLLATERAL-RATIO))
        )
      (asserts! (<= new-debt max-borrow)
                ERR-INSUFFICIENT-COLLATERAL)
      (asserts! (>= (get-available-liquidity) amount)
                ERR-INSUFFICIENT-BALANCE)

      (map-set user-loans { user: tx-sender } {
        principal-amount: new-debt,
        borrow-time: stacks-block-time,
        last-interest-update: stacks-block-time,
      })

      (try! (stx-transfer? amount CONTRACT-ADDRESS tx-sender))
      (var-set total-borrows (+ (var-get total-borrows) amount))

      (ok true)
    )
  )
)

(define-public (repay (amount uint))
  (let ((loan (unwrap! (map-get? user-loans { user: tx-sender })
             ERR-LOAN-NOT-FOUND)))
    (assert-not-paused)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    (let (
          (interest (accrue-interest tx-sender))
          (updated-loan (unwrap-panic
            (map-get? user-loans { user: tx-sender })))
          (total-debt (get principal-amount updated-loan))
        )
      (asserts! (<= amount total-debt) ERR-INVALID-AMOUNT)

      (try! (stx-transfer? amount tx-sender CONTRACT-ADDRESS))

      (if (>= amount total-debt)
          (begin
            (map-delete user-loans { user: tx-sender })
            (var-set total-borrows (- (var-get total-borrows) total-debt))
          )
          (begin
            (map-set user-loans { user: tx-sender } {
              principal-amount: (- total-debt amount),
              borrow-time: (get borrow-time updated-loan),
              last-interest-update: stacks-block-time,
            })
            (var-set total-borrows (- (var-get total-borrows) amount))
          )
      )

      (ok true)
    )
  )
)

;; ============================================================
;; LIQUIDATION
;; ============================================================

(define-public (liquidate
  (borrower principal)
  (liquidator <liquidator-trait>)
)
  (begin
    (assert-not-paused)

    (asserts!
      (is-eq true
        (match (var-get verified-liquidator-hash)
          expected
            (match (contract-hash? (contract-of liquidator))
              current (is-eq current expected)
              false)
          false))
      ERR-NOT-VERIFIED)

    (let (
          (loan (unwrap! (map-get? user-loans { user: borrower })
                 ERR-LOAN-NOT-FOUND))
          (coll (unwrap! (map-get? user-collateral { user: borrower })
                 ERR-LOAN-NOT-FOUND))
          (interest (unwrap-panic (calculate-current-interest borrower)))
          (total-debt (+ (get principal-amount loan) interest))
          (health (calculate-health-factor borrower))
        )
      (asserts! (< health MIN-HEALTH-FACTOR)
                ERR-POSITION-HEALTHY)

      (let ((liquidation-amount
              (min
                (+ total-debt
                   (/ (* total-debt LIQUIDATION-BONUS) u100))
                (get amount coll)
              )))
        (try! (contract-call? liquidator liquidate borrower total-debt))
        (try! (stx-transfer? liquidation-amount CONTRACT-ADDRESS
                             (contract-of liquidator)))

        (map-delete user-loans { user: borrower })
        (map-delete user-collateral { user: borrower })

        (var-set total-borrows (- (var-get total-borrows) total-debt))

        (ok true)
      )
    )
  )
)

;; ============================================================
;; ADMIN
;; ============================================================

(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-OWNER-ONLY)
    (var-set admin new-admin)
    (ok true)
  )
)

(define-public (set-paused (paused bool))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-OWNER-ONLY)
    (var-set protocol-paused paused)
    (ok true)
  )
)
