module GHC.WhyArentYouExported
  ( tcRnSrcDecls
  ) where

import Bag
import HsSyn
import HscTypes
import NameSet
import Outputable
import RnSource
import RnSplice ( rnTopSpliceDecls, traceSplice, SpliceInfo(..) )
import SrcLoc
import TcEnv
import TcEvidence
import TcHsSyn
import TcRnDriver
import TcRnMonad
import TcSimplify
import TcTypeable ( mkTypeableBinds )


tcRnSrcDecls :: [LHsDecl GhcPs]               -- Declarations
             -> TcM TcGblEnv
tcRnSrcDecls decls
 = do { -- Do all the declarations
      ; (tcg_env, tcl_env, lie) <- tc_rn_src_decls decls

        -- Check for the 'main' declaration
        -- Must do this inside the captureTopConstraints
      ; (tcg_env, lie_main) <- setEnvs (tcg_env, tcl_env) $
                               -- always set envs *before* captureTopConstraints
                               captureTopConstraints $
                               getGblEnv

      ; setEnvs (tcg_env, tcl_env) $ do {

             --         Simplify constraints
             --
             -- We do this after checkMain, so that we use the type info
             -- that checkMain adds
             --
             -- We do it with both global and local env in scope:
             --  * the global env exposes the instances to simplifyTop
             --  * the local env exposes the local Ids to simplifyTop,
             --    so that we get better error messages (monomorphism restriction)
      ; new_ev_binds <- {-# SCC "simplifyTop" #-}
                        simplifyTop (lie `andWC` lie_main)

        -- Emit Typeable bindings
      ; tcg_env <- mkTypeableBinds


      ; traceTc "Tc9" empty

      ; failIfErrsM     -- Don't zonk if there have been errors
                        -- It's a waste of time; and we may get debug warnings
                        -- about strangely-typed TyCons!
      ; traceTc "Tc10" empty

        -- Zonk the final code.  This must be done last.
        -- Even simplifyTop may do some unification.
        -- This pass also warns about missing type signatures
      ; (bind_env, ev_binds', binds', fords', imp_specs', rules')
            <- zonkTcGblEnv new_ev_binds tcg_env

        -- Finalizers must run after constraints are simplified, or some types
        -- might not be complete when using reify (see #12777).
        -- and also after we zonk the first time because we run typed splices
        -- in the zonker which gives rise to the finalisers.
      ; (tcg_env_mf, _) <- setGblEnv (clearTcGblEnv tcg_env)
                                     getEnvs
      ; traceTc "Tc11" empty

      ; -- zonk the new bindings arising from running the finalisers.
        -- This won't give rise to any more finalisers as you can't nest
        -- finalisers inside finalisers.
      ; (bind_env_mf, ev_binds_mf, binds_mf, fords_mf, imp_specs_mf, rules_mf)
            <- zonkTcGblEnv emptyBag tcg_env_mf


      ; let { final_type_env = plusTypeEnv (tcg_type_env tcg_env)
                                (plusTypeEnv bind_env_mf bind_env)
            ; tcg_env' = tcg_env_mf
                          { tcg_binds    = binds' `unionBags` binds_mf,
                            tcg_ev_binds = ev_binds' `unionBags` ev_binds_mf ,
                            tcg_imp_specs = imp_specs' ++ imp_specs_mf ,
                            tcg_rules    = rules' ++ rules_mf ,
                            tcg_fords    = fords' ++ fords_mf } } ;

      ; setGlobalTypeEnv tcg_env' final_type_env

   } }

zonkTcGblEnv :: Bag EvBind -> TcGblEnv
             -> TcM (TypeEnv, Bag EvBind, LHsBinds GhcTc,
                       [LForeignDecl GhcTc], [LTcSpecPrag], [LRuleDecl GhcTc])
zonkTcGblEnv new_ev_binds tcg_env =
  let TcGblEnv {   tcg_binds     = binds,
                   tcg_ev_binds  = cur_ev_binds,
                   tcg_imp_specs = imp_specs,
                   tcg_rules     = rules,
                   tcg_fords     = fords } = tcg_env

      all_ev_binds = cur_ev_binds `unionBags` new_ev_binds

  in {-# SCC "zonkTopDecls" #-}
      zonkTopDecls all_ev_binds binds rules imp_specs fords


-- | Remove accumulated bindings, rules and so on from TcGblEnv
clearTcGblEnv :: TcGblEnv -> TcGblEnv
clearTcGblEnv tcg_env
  = tcg_env { tcg_binds    = emptyBag,
              tcg_ev_binds = emptyBag ,
              tcg_imp_specs = [],
              tcg_rules    = [],
              tcg_fords    = [] }

tc_rn_src_decls :: [LHsDecl GhcPs]
                -> TcM (TcGblEnv, TcLclEnv, WantedConstraints)
-- Loops around dealing with each top level inter-splice group
-- in turn, until it's dealt with the entire module
-- Never emits constraints; calls captureTopConstraints internally
tc_rn_src_decls ds
 = {-# SCC "tc_rn_src_decls" #-}
   do { (first_group, group_tail) <- findSplice ds
                -- If ds is [] we get ([], Nothing)

        -- Deal with decls up to, but not including, the first splice
      ; (tcg_env, rn_decls) <- rnTopSrcDecls first_group
                -- rnTopSrcDecls fails if there are any errors

        -- Get TH-generated top-level declarations and make sure they don't
        -- contain any splices since we don't handle that at the moment
        --
        -- The plumbing here is a bit odd: see Trac #10853
      ; th_topdecls_var <- fmap tcg_th_topdecls getGblEnv
      ; th_ds <- readTcRef th_topdecls_var
      ; writeTcRef th_topdecls_var []

      ; (tcg_env, rn_decls) <-
            if null th_ds
            then return (tcg_env, rn_decls)
            else do { (th_group, th_group_tail) <- findSplice th_ds
                    ; case th_group_tail of
                        { Nothing -> return ()
                        ; Just (SpliceDecl _ (L loc _) _, _) ->
                            setSrcSpan loc
                            $ addErr (text
                                ("Declaration splices are not "
                                  ++ "permitted inside top-level "
                                  ++ "declarations added with addTopDecls"))
                        ; Just (XSpliceDecl _, _) -> panic "tc_rn_src_decls"
                        }
                      -- Rename TH-generated top-level declarations
                    ; (tcg_env, th_rn_decls) <- setGblEnv tcg_env
                        $ rnTopSrcDecls th_group

                      -- Dump generated top-level declarations
                    ; let msg = "top-level declarations added with addTopDecls"
                    ; traceSplice
                        $ SpliceInfo { spliceDescription = msg
                                     , spliceIsDecl    = True
                                     , spliceSource    = Nothing
                                     , spliceGenerated = ppr th_rn_decls }
                    ; return (tcg_env, appendGroups rn_decls th_rn_decls)
                    }

      -- Type check all declarations
      -- NB: set the env **before** captureTopConstraints so that error messages
      -- get reported w.r.t. the right GlobalRdrEnv. It is for this reason that
      -- the captureTopConstraints must go here, not in tcRnSrcDecls.
      ; ((tcg_env, tcl_env), lie1) <- setGblEnv tcg_env $
                                      captureTopConstraints $
                                      tcTopSrcDecls rn_decls

        -- If there is no splice, we're nearly done
      ; setEnvs (tcg_env, tcl_env) $
        case group_tail of
          { Nothing -> return (tcg_env, tcl_env, lie1)

            -- If there's a splice, we must carry on
          ; Just (SpliceDecl _ (L loc splice) _, rest_ds) ->
            do { recordTopLevelSpliceLoc loc

                 -- Rename the splice expression, and get its supporting decls
               ; (spliced_decls, splice_fvs) <- rnTopSpliceDecls splice

                 -- Glue them on the front of the remaining decls and loop
               ; (tcg_env, tcl_env, lie2) <-
                   setGblEnv (tcg_env `addTcgDUs` usesOnly splice_fvs) $
                   tc_rn_src_decls (spliced_decls ++ rest_ds)

               ; return (tcg_env, tcl_env, lie1 `andWC` lie2)
               }
          ; Just (XSpliceDecl _, _) -> panic "tc_rn_src_decls"
          }
      }
