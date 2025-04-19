(: import_module_source (IdPath -> (Result () String)))
(= import_module_source
  id_path (do
    (:= rel_path    (fold_id_path id_path))
    (:= source_name (match (to_str rel_path)
      (Just str) (pure str)
      _          (Exception (format "Failed to convert path {} to string" rel_path))
    ))
    (:= source (match (config .(import_src rel_path))
      (Ok src) (pure src)
      (Err e)  (Exception (format "Failed to import module from {} with error: {}" 
                              rel_path e))
    ))
    ;; Add the source to the context
    (modify-context 
      (lambda (ctx) (Context 
        (= staged_srcs (Cons source (ctx .staged_srcs)))
        (= staged_src_names (Cons source_name (ctx .staged_src_names)))
        ..ctx
      ))
    )
    (pure ())
  )
)

(: fold_id_path (IdPath -> (Result PathBuf String)))
(= fold_id_path
  id_path (fold_m
    (lambda (path i) (do
    ;; At first, how should multiple value matching should be differentiated from application expression?
    ;; Should I depend on the parsed airity of identifier again?
    ;; Or should I use match with a single data?
    ;; On matching multiple values, one should either use function or tuple
    ;; Which makes pattern matching is somewhat second class
    ;; Or maybe pattern matching on inlined tuple should be guaranteed to compile down to 
      (match (resolve_ident i)
        (Just name) (pure (path .(push_back name)))
        _           (Exception (format 
                      "While importing module {}, ident {} failed to resolve" 
                      id_path i
                    ))
      )
    ))
    (Path::new)
    (get-idents id_path)
  )
)