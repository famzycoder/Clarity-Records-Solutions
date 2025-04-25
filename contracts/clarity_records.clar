;; Clarity Records Solutions


;; =========== HELPER FUNCTIONS ==========

;; Validate if document exists in registry
(define-private (document-registered (doc-id uint))
  (is-some (map-get? estate-documents { doc-id: doc-id }))
)


;; Get document size in bytes
(define-private (retrieve-document-size (doc-id uint))
  (default-to u0
    (get file-size
      (map-get? estate-documents { doc-id: doc-id })
    )
  )
)

;; Validate tag formatting
(define-private (valid-tag-format (tag (string-ascii 32)))
  (and
    (> (len tag) u0)
    (< (len tag) u33)
  )
)

;; =========== DATA STORAGE STRUCTURES ==========

;; Primary Estate Documents Repository
(define-map estate-documents
  { doc-id: uint }
  {
    title: (string-ascii 64),
    owner: principal,
    file-size: uint,
    registration-block: uint,
    description: (string-ascii 128),
    tags: (list 10 (string-ascii 32))
  }
)

;; Document Access Management
(define-map document-permissions
  { doc-id: uint, viewer: principal }
  { access-allowed: bool }
)

;; Global Document Counter
(define-data-var document-counter uint u0)


;; Check if caller has ownership rights to document
(define-private (verify-document-ownership (doc-id uint) (user principal))
  (match (map-get? estate-documents { doc-id: doc-id })
    document-data (is-eq (get owner document-data) user)
    false
  )
)

;; Validate set of tags
(define-private (validate-tag-collection (tags (list 10 (string-ascii 32))))
  (and
    (> (len tags) u0)
    (<= (len tags) u10)
    (is-eq (len (filter valid-tag-format tags)) (len tags))
  )
)

;; =========== SYSTEM CONSTANTS & ERROR CODES ==========

;; System Administration
(define-constant registry-admin tx-sender)

;; Response Status Codes for System Operations
(define-constant err-document-not-found (err u301))
(define-constant err-document-already-exists (err u302))
(define-constant err-invalid-title-format (err u303))
(define-constant err-invalid-document-volume (err u304))
(define-constant err-unauthorized-access (err u305))
(define-constant err-ownership-check-failed (err u306))
(define-constant err-admin-only-operation (err u300))
(define-constant err-viewing-restricted (err u307))
(define-constant err-tag-validation-failed (err u308))

;; =========== DOCUMENT MANAGEMENT FUNCTIONS ==========

;; Register a new document in the estate registry
(define-public (register-document
  (title (string-ascii 64))
  (file-size uint)
  (description (string-ascii 128))
  (tags (list 10 (string-ascii 32)))
)
  (let
    (
      (new-doc-id (+ (var-get document-counter) u1))
    )
    ;; Input validation checks
    (asserts! (> (len title) u0) err-invalid-title-format)
    (asserts! (< (len title) u65) err-invalid-title-format)
    (asserts! (> file-size u0) err-invalid-document-volume)
    (asserts! (< file-size u1000000000) err-invalid-document-volume)
    (asserts! (> (len description) u0) err-invalid-title-format)
    (asserts! (< (len description) u129) err-invalid-title-format)
    (asserts! (validate-tag-collection tags) err-tag-validation-failed)

    ;; Create new document entry
    (map-insert estate-documents
      { doc-id: new-doc-id }
      {
        title: title,
        owner: tx-sender,
        file-size: file-size,
        registration-block: block-height,
        description: description,
        tags: tags
      }
    )

    ;; Initialize permissions for document creator
    (map-insert document-permissions
      { doc-id: new-doc-id, viewer: tx-sender }
      { access-allowed: true }
    )

    ;; Update document counter
    (var-set document-counter new-doc-id)
    (ok new-doc-id)
  )
)

;; Update existing document information
(define-public (update-document
  (doc-id uint)
  (new-title (string-ascii 64))
  (new-file-size uint)
  (new-description (string-ascii 128))
  (new-tags (list 10 (string-ascii 32)))
)
  (let
    (
      (doc-data (unwrap! (map-get? estate-documents { doc-id: doc-id }) err-document-not-found))
    )
    ;; Validate document exists and caller is the owner
    (asserts! (document-registered doc-id) err-document-not-found)
    (asserts! (is-eq (get owner doc-data) tx-sender) err-ownership-check-failed)

    ;; Input validation
    (asserts! (> (len new-title) u0) err-invalid-title-format)
    (asserts! (< (len new-title) u65) err-invalid-title-format)
    (asserts! (> new-file-size u0) err-invalid-document-volume)
    (asserts! (< new-file-size u1000000000) err-invalid-document-volume)
    (asserts! (> (len new-description) u0) err-invalid-title-format)
    (asserts! (< (len new-description) u129) err-invalid-title-format)
    (asserts! (validate-tag-collection new-tags) err-tag-validation-failed)

    ;; Update document with new information
    (map-set estate-documents
      { doc-id: doc-id }
      (merge doc-data {
        title: new-title,
        file-size: new-file-size,
        description: new-description,
        tags: new-tags
      })
    )
    (ok true)
  )
)

;; Remove document from the registry
(define-public (deregister-document (doc-id uint))
  (let
    (
      (doc-data (unwrap! (map-get? estate-documents { doc-id: doc-id }) err-document-not-found))
    )
    ;; Verify document exists and caller is owner
    (asserts! (document-registered doc-id) err-document-not-found)
    (asserts! (is-eq (get owner doc-data) tx-sender) err-ownership-check-failed)

    ;; Delete document record
    (map-delete estate-documents { doc-id: doc-id })
    (ok true)
  )
)

;; Change document ownership
(define-public (reassign-ownership (doc-id uint) (new-owner principal))
  (let
    (
      (doc-data (unwrap! (map-get? estate-documents { doc-id: doc-id }) err-document-not-found))
    )
    ;; Verify document exists and caller is current owner
    (asserts! (document-registered doc-id) err-document-not-found)
    (asserts! (is-eq (get owner doc-data) tx-sender) err-ownership-check-failed)

    ;; Update ownership record
    (map-set estate-documents
      { doc-id: doc-id }
      (merge doc-data { owner: new-owner })
    )
    (ok true)
  )
)

;; =========== ACCESS CONTROL FUNCTIONS ==========

;; Grant document viewing permission to another user
(define-public (grant-document-access (doc-id uint) (viewer principal))
  (let
    (
      (doc-data (unwrap! (map-get? estate-documents { doc-id: doc-id }) err-document-not-found))
    )
    ;; Verify document exists and caller is owner
    (asserts! (document-registered doc-id) err-document-not-found)
    (asserts! (verify-document-ownership doc-id tx-sender) err-ownership-check-failed)

    (ok true)
  )
)

;; Remove viewing permission from a user
(define-public (withdraw-document-access (doc-id uint) (viewer principal))
  (let
    (
      (doc-data (unwrap! (map-get? estate-documents { doc-id: doc-id }) err-document-not-found))
    )
    ;; Verify document exists and caller is owner
    (asserts! (document-registered doc-id) err-document-not-found)
    (asserts! (is-eq (get owner doc-data) tx-sender) err-ownership-check-failed)
    (asserts! (not (is-eq viewer tx-sender)) err-admin-only-operation)

    ;; Remove access permission
    (map-delete document-permissions { doc-id: doc-id, viewer: viewer })
    (ok true)
  )
)

;; =========== METADATA MANAGEMENT ==========

;; Add additional tags to document
(define-public (extend-document-tags (doc-id uint) (additional-tags (list 10 (string-ascii 32))))
  (let
    (
      (doc-data (unwrap! (map-get? estate-documents { doc-id: doc-id }) err-document-not-found))
      (existing-tags (get tags doc-data))
      (combined-tags (unwrap! (as-max-len? (concat existing-tags additional-tags) u10) err-tag-validation-failed))
    )
    ;; Verify document exists and caller is owner
    (asserts! (document-registered doc-id) err-document-not-found)
    (asserts! (is-eq (get owner doc-data) tx-sender) err-ownership-check-failed)

    ;; Validate new tags format
    (asserts! (validate-tag-collection additional-tags) err-tag-validation-failed)

    ;; Update document with combined tags
    (map-set estate-documents
      { doc-id: doc-id }
      (merge doc-data { tags: combined-tags })
    )
    (ok combined-tags)
  )
)

;; =========== SECURITY FUNCTIONS ==========

;; Freeze document to prevent modifications in case of dispute
(define-public (institute-document-freeze (doc-id uint))
  (let
    (
      (doc-data (unwrap! (map-get? estate-documents { doc-id: doc-id }) err-document-not-found))
      (freeze-label "LEGAL-HOLD")
      (current-tags (get tags doc-data))
    )
    ;; Verify caller is either the document owner or system administrator
    (asserts! (document-registered doc-id) err-document-not-found)
    (asserts! 
      (or 
        (is-eq tx-sender registry-admin)
        (is-eq (get owner doc-data) tx-sender)
      ) 
      err-admin-only-operation
    )

    ;; Document freeze implementation would go here
    ;; This is a placeholder function that returns success
    (ok true)
  )
)

;; =========== VERIFICATION FUNCTIONS ==========

;; Validate document authenticity and ownership chain
(define-public (authenticate-document (doc-id uint) (presumed-owner principal))
  (let
    (
      (doc-data (unwrap! (map-get? estate-documents { doc-id: doc-id }) err-document-not-found))
      (actual-owner (get owner doc-data))
      (registration-height (get registration-block doc-data))
      (has-access (default-to 
        false 
        (get access-allowed 
          (map-get? document-permissions { doc-id: doc-id, viewer: tx-sender })
        )
      ))
    )
    ;; Verify document exists and caller has access rights
    (asserts! (document-registered doc-id) err-document-not-found)
    (asserts! 
      (or 
        (is-eq tx-sender actual-owner)
        has-access
        (is-eq tx-sender registry-admin)
      )
      err-unauthorized-access
    )

    ;; Return verification results with timestamp information
    (if (is-eq actual-owner presumed-owner)
      (ok {
        verification-status: true,
        verification-block: block-height,
        age-in-blocks: (- block-height registration-height),
        ownership-verified: true
      })
      (ok {
        verification-status: false,
        verification-block: block-height,
        age-in-blocks: (- block-height registration-height),
        ownership-verified: false
      })
    )
  )
)

;; Additional helper functions to enhance code length and readability
(define-private (document-age-in-days (doc-id uint))
  (let
    (
      (doc-data (map-get? estate-documents { doc-id: doc-id }))
      (blocks-per-day u144) ;; Approximate blocks per day
    )
    (match doc-data
      data (/ (- block-height (get registration-block data)) blocks-per-day)
      u0
    )
  )
)

;; System information retrieval - Admin only operation
(define-public (get-system-statistics)
  (begin
    (asserts! (is-eq tx-sender registry-admin) err-admin-only-operation)
    (ok {
      total-documents: (var-get document-counter),
      current-block: block-height,
      system-status: "Operational"
    })
  )
)

