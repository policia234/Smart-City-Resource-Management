;; Smart City Resource Management - Core Contract
;; Manages urban resources with dynamic pricing and citizen feedback

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_RESOURCE_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u102))
(define-constant ERR_RESOURCE_UNAVAILABLE (err u103))
(define-constant ERR_INVALID_DURATION (err u104))
(define-constant ERR_RESERVATION_NOT_FOUND (err u105))
(define-constant ERR_INVALID_RATING (err u106))
(define-constant ERR_ALREADY_RATED (err u107))

;; Resource types
(define-constant RESOURCE_PARKING u1)
(define-constant RESOURCE_BIKE_SHARE u2)
(define-constant RESOURCE_PUBLIC_FACILITY u3)

;; Data structures
(define-map resources
  { resource-id: uint }
  {
    resource-type: uint,
    name: (string-ascii 64),
    location: (string-ascii 128),
    capacity: uint,
    available: uint,
    base-price: uint,
    dynamic-multiplier: uint,
    total-ratings: uint,
    rating-sum: uint,
    is-active: bool
  }
)

(define-map reservations
  { reservation-id: uint }
  {
    user: principal,
    resource-id: uint,
    start-block: uint,
    duration-blocks: uint,
    amount-paid: uint,
    is-active: bool
  }
)

(define-map user-ratings
  { user: principal, resource-id: uint }
  { has-rated: bool }
)

;; Counters
(define-data-var next-resource-id uint u1)
(define-data-var next-reservation-id uint u1)

;; Admin functions
(define-public (add-resource
  (resource-type uint)
  (name (string-ascii 64))
  (location (string-ascii 128))
  (capacity uint)
  (base-price uint))
  (let ((resource-id (var-get next-resource-id)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set resources
      { resource-id: resource-id }
      {
        resource-type: resource-type,
        name: name,
        location: location,
        capacity: capacity,
        available: capacity,
        base-price: base-price,
        dynamic-multiplier: u100, ;; 100 = 1.0x multiplier
        total-ratings: u0,
        rating-sum: u0,
        is-active: true
      }
    )
    (var-set next-resource-id (+ resource-id u1))
    (ok resource-id)
  )
)

(define-public (update-resource-availability (resource-id uint) (new-availability uint))
  (let ((resource (unwrap! (map-get? resources { resource-id: resource-id }) ERR_RESOURCE_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-availability (get capacity resource)) ERR_RESOURCE_UNAVAILABLE)
    (map-set resources
      { resource-id: resource-id }
      (merge resource { available: new-availability })
    )
    (ok true)
  )
)

;; Dynamic pricing calculation
(define-private (calculate-dynamic-price (resource-id uint))
  (match (map-get? resources { resource-id: resource-id })
    resource (let ((utilization-rate (if (> (get capacity resource) u0)
                                     (/ (* (- (get capacity resource) (get available resource)) u100)
                                        (get capacity resource))
                                     u0)))
              ;; Dynamic multiplier based on utilization: 50% = 1.0x, 90% = 2.0x, 100% = 3.0x
              (let ((multiplier (if (< utilization-rate u50)
                                   u100  ;; 1.0x
                                   (if (< utilization-rate u75)
                                       u125  ;; 1.25x
                                       (if (< utilization-rate u90)
                                           u150  ;; 1.5x
                                           u200))))) ;; 2.0x
                (/ (* (get base-price resource) multiplier) u100)
              )
            )
    u0 ;; Return 0 if resource not found
  )
)

;; Reservation functions
(define-public (make-reservation (resource-id uint) (duration-blocks uint))
  (let (
    (resource (unwrap! (map-get? resources { resource-id: resource-id }) ERR_RESOURCE_NOT_FOUND))
    (current-price (calculate-dynamic-price resource-id))
    (total-cost (* current-price duration-blocks))
    (reservation-id (var-get next-reservation-id))
  )
    (asserts! (get is-active resource) ERR_RESOURCE_UNAVAILABLE)
    (asserts! (> (get available resource) u0) ERR_RESOURCE_UNAVAILABLE)
    (asserts! (> duration-blocks u0) ERR_INVALID_DURATION)

    ;; Transfer payment (in production, this would handle STX transfers)
    ;; For now, we'll assume payment is handled externally

    ;; Update resource availability
    (map-set resources
      { resource-id: resource-id }
      (merge resource { available: (- (get available resource) u1) })
    )

    ;; Create reservation
    (map-set reservations
      { reservation-id: reservation-id }
      {
        user: tx-sender,
        resource-id: resource-id,
        start-block: stacks-block-height,
        duration-blocks: duration-blocks,
        amount-paid: total-cost,
        is-active: true
      }
    )

    (var-set next-reservation-id (+ reservation-id u1))
    (ok reservation-id)
  )
)

(define-public (end-reservation (reservation-id uint))
  (let ((reservation (unwrap! (map-get? reservations { reservation-id: reservation-id }) ERR_RESERVATION_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get user reservation)) ERR_UNAUTHORIZED)
    (asserts! (get is-active reservation) ERR_RESERVATION_NOT_FOUND)

    ;; Update reservation status
    (map-set reservations
      { reservation-id: reservation-id }
      (merge reservation { is-active: false })
    )

    ;; Return resource to available pool
    (let ((resource (unwrap! (map-get? resources { resource-id: (get resource-id reservation) }) ERR_RESOURCE_NOT_FOUND)))
      (map-set resources
        { resource-id: (get resource-id reservation) }
        (merge resource { available: (+ (get available resource) u1) })
      )
    )

    (ok true)
  )
)

;; Citizen feedback system
(define-public (rate-resource (resource-id uint) (rating uint))
  (let ((resource (unwrap! (map-get? resources { resource-id: resource-id }) ERR_RESOURCE_NOT_FOUND)))
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
    (asserts! (is-none (map-get? user-ratings { user: tx-sender, resource-id: resource-id })) ERR_ALREADY_RATED)

    ;; Record that user has rated this resource
    (map-set user-ratings
      { user: tx-sender, resource-id: resource-id }
      { has-rated: true }
    )

    ;; Update resource rating
    (map-set resources
      { resource-id: resource-id }
      (merge resource {
        total-ratings: (+ (get total-ratings resource) u1),
        rating-sum: (+ (get rating-sum resource) rating)
      })
    )

    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-resource (resource-id uint))
  (map-get? resources { resource-id: resource-id })
)

(define-read-only (get-reservation (reservation-id uint))
  (map-get? reservations { reservation-id: reservation-id })
)

(define-read-only (get-current-price (resource-id uint))
  (calculate-dynamic-price resource-id)
)

(define-read-only (get-resource-rating (resource-id uint))
  (let ((resource (unwrap! (map-get? resources { resource-id: resource-id }) ERR_RESOURCE_NOT_FOUND)))
    (if (> (get total-ratings resource) u0)
      (ok (/ (get rating-sum resource) (get total-ratings resource)))
      (ok u0)
    )
  )
)

(define-read-only (get-utilization-rate (resource-id uint))
  (let ((resource (unwrap! (map-get? resources { resource-id: resource-id }) ERR_RESOURCE_NOT_FOUND)))
    (if (> (get capacity resource) u0)
      (ok (/ (* (- (get capacity resource) (get available resource)) u100) (get capacity resource)))
      (ok u0)
    )
  )
)

(define-read-only (has-user-rated (user principal) (resource-id uint))
  (is-some (map-get? user-ratings { user: user, resource-id: resource-id }))
)

;; Get active reservations for a user
(define-read-only (get-user-reservations (user principal))
  ;; In a full implementation, this would iterate through reservations
  ;; For now, returns a simple check format
  (ok "Use get-reservation with specific reservation-id")
)

;; Emergency functions
(define-public (emergency-shutdown (resource-id uint))
  (let ((resource (unwrap! (map-get? resources { resource-id: resource-id }) ERR_RESOURCE_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set resources
      { resource-id: resource-id }
      (merge resource { is-active: false, available: u0 })
    )
    (ok true)
  )
)

(define-public (reactivate-resource (resource-id uint))
  (let ((resource (unwrap! (map-get? resources { resource-id: resource-id }) ERR_RESOURCE_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set resources
      { resource-id: resource-id }
      (merge resource { is-active: true, available: (get capacity resource) })
    )
    (ok true)
  )
)
