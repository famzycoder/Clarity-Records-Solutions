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


