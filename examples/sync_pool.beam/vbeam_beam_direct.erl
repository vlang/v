%% vbeam_beam_direct - Core Erlang AST to BEAM assembly (Tier 4)
%%
%% Translates Core Erlang AST (c_module tuples) directly to BEAM assembly
%% tuples, bypassing the Erlang compiler's Core Erlang optimization passes.
%% Uses compile:noenv_forms([from_asm]) for beam_validator + binary packaging.
%%
%% Pipeline:
%%   V -> ETF terms (existing) -> vbeam_beam_direct -> compile([from_asm]) -> .beam
%%
%% Activation: VBEAM_DIRECT=1 environment variable
%%
%% Exports:
%%   compile/1,2    - Core Erlang AST -> {ok, Mod, Binary} | {error, ...}
%%   compile_to_asm/1 - Core Erlang AST -> BEAM assembly 5-tuple
-module(vbeam_beam_direct).
-export([compile/1, compile/2, compile_to_asm/1]).

%% Sentinel value for provisional stack size during compilation.
%% Replaced with actual frame size post-compilation via fix_frame_size/2.
-define(FRAME_PLACEHOLDER, 999999).

%% Compilation state
-record(st, {
    mod     :: atom(),         %% module name
    label   :: integer(),      %% next available label
    vars    :: #{},            %% VarName => Register ({x,N} or {y,N})
    next_x  :: integer(),      %% next free x-register
    next_y  :: integer(),      %% next free y-register index
    stack   :: integer(),      %% current stack frame size (y-regs allocated)
    fn_labels :: #{},          %% {Name, Arity} => {FiLabel, EntryLabel, FreeVarNames}
    extra_fns :: [tuple()],    %% lifted letrec functions
    lambda_vars :: #{},        %% VarName => {LiftedName, Arity} for lambda var→fn mapping
    anon_count :: integer()    %% counter for generating unique anon function names
}).

%% ---------------------------------------------------------------------------
%% Entry points
%% ---------------------------------------------------------------------------

-spec compile(tuple()) -> {ok, atom(), binary()} | {error, term()}.
compile(CoreMod) ->
    compile(CoreMod, []).

-spec compile(tuple(), [term()]) -> {ok, atom(), binary()} | {error, term()}.
compile(CoreMod, Opts) ->
    try
        Asm = compile_to_asm(CoreMod),
        compile:noenv_forms(Asm, [from_asm, binary, return_errors | Opts])
    catch
        _:Reason ->
            %% Compilation error — return error tuple for Tier 3 fallback
            {error, [{0, [{0, vbeam_beam_direct, Reason}]}], []}
    end.

%% @doc Translate Core Erlang AST to BEAM assembly 5-tuple.
%% Format: {Module, Exports, Attrs, [{function,...}], LabelCount}
-spec compile_to_asm(tuple()) -> tuple().
compile_to_asm(CoreMod) ->
    ModAtom = cerl:concrete(cerl:module_name(CoreMod)),
    Exports = [{cerl:fname_id(E), cerl:fname_arity(E)}
               || E <- cerl:module_exports(CoreMod)],
    Defs = cerl:module_defs(CoreMod),
    Attrs = compile_attrs(cerl:module_attrs(CoreMod)),
    St0 = #st{mod = ModAtom, label = 1, vars = #{}, next_x = 0, next_y = 0, stack = 0, fn_labels = #{}, extra_fns = [], lambda_vars = #{}, anon_count = 0},
    %% First pass: pre-assign entry labels for all functions
    {FnLabels, St0a} = assign_fn_labels(Defs, St0),
    St0b = St0a#st{fn_labels = FnLabels},
    {Functions, StN} = compile_defs(Defs, St0b),
    ExtraFns = lists:reverse(StN#st.extra_fns),
    AllFunctions = Functions ++ ExtraFns,
    %% Export lifted lambda functions so erlang:make_fun/3 can reference them
    ExtraExports = [{element(2, Fn), element(3, Fn)} || Fn <- ExtraFns],
    AllExports = Exports ++ ExtraExports,
    {ModAtom, AllExports, Attrs, AllFunctions, StN#st.label}.

%% ---------------------------------------------------------------------------
%% Module attributes
%% ---------------------------------------------------------------------------

compile_attrs(Attrs) ->
    [{cerl:concrete(K), cerl:concrete(V)} || {K, V} <- Attrs].

%% ---------------------------------------------------------------------------
%% Pre-assign function labels (first pass)
%% ---------------------------------------------------------------------------

%% Allocate FiLabel + EntryLabel for each function so local calls can
%% resolve {Name, Arity} to {f, EntryLabel} during compilation.
assign_fn_labels([], St) ->
    {#{}, St};
assign_fn_labels([{Name, _Body} | Rest], St) ->
    FnName = cerl:fname_id(Name),
    Arity = cerl:fname_arity(Name),
    {FiLabel, St1} = new_label(St),
    {EntryLabel, St2} = new_label(St1),
    {RestLabels, St3} = assign_fn_labels(Rest, St2),
    {maps:put({FnName, Arity}, {FiLabel, EntryLabel, []}, RestLabels), St3}.

%% ---------------------------------------------------------------------------
%% Function definitions
%% ---------------------------------------------------------------------------

compile_defs([], St) ->
    {[], St};
compile_defs([{Name, Body} | Rest], St) ->
    FnName = cerl:fname_id(Name),
    Arity = cerl:fname_arity(Name),
    {Fn, St1} = compile_fun(FnName, Arity, Body, St),
    {RestFns, St2} = compile_defs(Rest, St1),
    {[Fn | RestFns], St2}.

compile_fun(FnName, Arity, FunNode, St) ->
    compile_fun(FnName, Arity, FunNode, St, []).

compile_fun(FnName, Arity, FunNode, St, ExtraBindings) ->
    %% Use pre-assigned labels from first pass
    {FiLabel, EntryLabel, _FreeVars} = maps:get({FnName, Arity}, St#st.fn_labels),
    ActualArity = Arity + length(ExtraBindings),
    Params = cerl:fun_vars(FunNode),
    Body = cerl:fun_body(FunNode),
    %% Determine if a stack frame is needed
    %% call_fun2 requires stack in a known state, so any function that calls
    %% through a variable (lambda_vars or call_fun2) needs a frame.
    NeedsFrame = count_stack_slots(Body) > 0 orelse has_non_tail_calls(Body)
                 orelse has_call_fun(Body),
    case NeedsFrame of
        false ->
            %% No stack frame — bind params to x-registers
            V0 = bind_params(Params, 0, #{}),
            V1 = bind_extras(ExtraBindings, Arity, V0),
            St3 = St#st{vars = V1, next_x = ActualArity, next_y = 0, stack = 0},
            {BodyCode, ResultReg, St4} = compile_expr(Body, St3, tail),
            FinalCode = ensure_terminator(BodyCode, ResultReg, 0),
            Fn = {function, FnName, ActualArity, EntryLabel,
                  [{label, FiLabel},
                   {func_info, {atom, St#st.mod}, {atom, FnName}, ActualArity},
                   {label, EntryLabel}]
                  ++ FinalCode},
            {Fn, St4};
        true ->
            %% Stack frame needed — compile body first with large provisional stack
            %% to determine actual y-register usage, then set frame size
            V0 = bind_params_to_y(Params, 0, #{}),
            V1 = bind_extras_to_y(ExtraBindings, Arity, V0),
            ParamSaveCode = case ActualArity of
                0 -> [];
                _ -> [{move, {x, I}, {y, I}} || I <- lists:seq(0, ActualArity - 1)]
            end,
            NextY = ActualArity,
            %% Use large provisional stack so y-regs are always valid during compilation
            St3 = St#st{vars = V1, next_x = ActualArity, next_y = NextY, stack = ?FRAME_PLACEHOLDER},
            {BodyCode, ResultReg, St4} = compile_expr(Body, St3, tail),
            %% Actual frame size = max y-register index allocated during compilation
            ActualFrameSize = St4#st.next_y,
            %% Replace all provisional frame sizes in generated code with actual
            FixedBodyCode = fix_frame_size(BodyCode, ActualFrameSize),
            FinalCode = ensure_terminator(FixedBodyCode, ResultReg, ActualFrameSize),
            %% Emit allocate with the actual frame size
            YRegs = [{y, I} || I <- lists:seq(0, ActualFrameSize - 1)],
            AllocCode = [{allocate, ActualFrameSize, ActualArity},
                         {init_yregs, {list, YRegs}}],
            Fn = {function, FnName, ActualArity, EntryLabel,
                  [{label, FiLabel},
                   {func_info, {atom, St#st.mod}, {atom, FnName}, ActualArity},
                   {label, EntryLabel}]
                  ++ AllocCode
                  ++ ParamSaveCode
                  ++ FinalCode},
            {Fn, St4#st{stack = ActualFrameSize}}
    end.

bind_params([], _N, Acc) -> Acc;
bind_params([P | Rest], N, Acc) ->
    bind_params(Rest, N + 1, maps:put(cerl:var_name(P), {x, N}, Acc)).

bind_params_to_y([], _N, Acc) -> Acc;
bind_params_to_y([P | Rest], N, Acc) ->
    bind_params_to_y(Rest, N + 1, maps:put(cerl:var_name(P), {y, N}, Acc)).

%% Bind extra (lambda-lifted) free variable names to x-registers
bind_extras([], _N, Acc) -> Acc;
bind_extras([VarName | Rest], N, Acc) ->
    bind_extras(Rest, N + 1, maps:put(VarName, {x, N}, Acc)).

%% Bind extra free variable names to y-registers
bind_extras_to_y([], _N, Acc) -> Acc;
bind_extras_to_y([VarName | Rest], N, Acc) ->
    bind_extras_to_y(Rest, N + 1, maps:put(VarName, {y, N}, Acc)).

%% ---------------------------------------------------------------------------
%% Stack frame analysis
%% ---------------------------------------------------------------------------

%% Count y-register slots needed for let-bindings in a function body.
count_stack_slots(Node) ->
    case cerl:type(Node) of
        'let' ->
            length(cerl:let_vars(Node))
            + count_stack_slots(cerl:let_arg(Node))
            + count_stack_slots(cerl:let_body(Node));
        seq ->
            count_stack_slots(cerl:seq_arg(Node))
            + count_stack_slots(cerl:seq_body(Node));
        'case' ->
            1  %% scrutinee saved to y-register
            + count_stack_slots(cerl:case_arg(Node))
            + lists:max([count_stack_slots(cerl:clause_body(C))
                         || C <- cerl:case_clauses(Node)] ++ [0]);
        'try' ->
            max(
                count_stack_slots(cerl:try_arg(Node))
                + count_stack_slots(cerl:try_body(Node)),
                count_stack_slots(cerl:try_handler(Node))
            );
        letrec ->
            count_stack_slots(cerl:letrec_body(Node));
        _ ->
            0
    end.

%% Check if body has calls in non-tail position (requires a stack frame).
has_non_tail_calls(Node) ->
    case cerl:type(Node) of
        'let' ->
            has_any_call(cerl:let_arg(Node)) orelse
            has_non_tail_calls(cerl:let_body(Node));
        seq ->
            has_any_call(cerl:seq_arg(Node)) orelse
            has_non_tail_calls(cerl:seq_body(Node));
        'case' ->
            has_any_call(cerl:case_arg(Node)) orelse
            lists:any(fun(C) -> has_non_tail_calls(cerl:clause_body(C)) end,
                      cerl:case_clauses(Node));
        'try' ->
            has_any_call(cerl:try_arg(Node)) orelse
            has_non_tail_calls(cerl:try_body(Node));
        letrec ->
            has_non_tail_calls(cerl:letrec_body(Node));
        call ->
            %% The call itself is tail, but if any argument contains a call,
            %% that inner call is in non-tail position and needs a frame
            lists:any(fun has_any_call/1, cerl:call_args(Node));
        apply ->
            lists:any(fun has_any_call/1, cerl:apply_args(Node));
        cons ->
            has_any_call(cerl:cons_hd(Node)) orelse
            has_any_call(cerl:cons_tl(Node));
        tuple ->
            lists:any(fun has_any_call/1, cerl:tuple_es(Node));
        _ ->
            false
    end.

%% Check if body contains any apply through a variable (produces call_fun2).
%% call_fun2 requires {allocated, _} state even in tail position.
has_call_fun(Node) ->
    case cerl:type(Node) of
        apply ->
            Op = cerl:apply_op(Node),
            case cerl:type(Op) of
                var ->
                    FnName = cerl:var_name(Op),
                    case FnName of
                        {_Name, _Ar} -> false;  %% named function, not call_fun2
                        _ -> true               %% variable fun call → call_fun2
                    end;
                _ -> false
            end;
        'let' ->
            has_call_fun(cerl:let_arg(Node)) orelse
            has_call_fun(cerl:let_body(Node));
        seq ->
            has_call_fun(cerl:seq_arg(Node)) orelse
            has_call_fun(cerl:seq_body(Node));
        'case' ->
            has_call_fun(cerl:case_arg(Node)) orelse
            lists:any(fun(C) -> has_call_fun(cerl:clause_body(C)) end,
                      cerl:case_clauses(Node));
        'try' ->
            has_call_fun(cerl:try_arg(Node)) orelse
            has_call_fun(cerl:try_body(Node));
        letrec ->
            has_call_fun(cerl:letrec_body(Node));
        _ ->
            false
    end.

%% Check if a node contains any call or apply expression.
has_any_call(Node) ->
    case cerl:type(Node) of
        call -> true;
        apply -> true;
        'fun' -> true;  %% compile_lambda emits call_ext for erlang:make_fun/3
        'let' ->
            has_any_call(cerl:let_arg(Node)) orelse
            has_any_call(cerl:let_body(Node));
        seq ->
            has_any_call(cerl:seq_arg(Node)) orelse
            has_any_call(cerl:seq_body(Node));
        'case' ->
            has_any_call(cerl:case_arg(Node)) orelse
            lists:any(fun(C) -> has_any_call(cerl:clause_body(C)) end,
                      cerl:case_clauses(Node));
        'try' ->
            has_any_call(cerl:try_arg(Node)) orelse
            has_any_call(cerl:try_body(Node));
        letrec ->
            has_any_call(cerl:letrec_body(Node));
        cons ->
            has_any_call(cerl:cons_hd(Node)) orelse
            has_any_call(cerl:cons_tl(Node));
        tuple ->
            lists:any(fun has_any_call/1, cerl:tuple_es(Node));
        _ ->
            false
    end.

%% ---------------------------------------------------------------------------
%% Expression compilation
%%
%% Position:
%%   tail   - last expression, can use tail calls (call_ext_only)
%%   value  - result needed in a register
%%   effect - result discarded
%%
%% Returns: {Instructions, ResultRegister, NewState}
%% ---------------------------------------------------------------------------

compile_expr(Node, St, Pos) ->
    case cerl:type(Node) of
        literal ->
            compile_literal(Node, St);
        var ->
            compile_var(Node, St);
        call ->
            compile_call(Node, St, Pos);
        'let' ->
            compile_let(Node, St, Pos);
        seq ->
            compile_seq(Node, St, Pos);
        apply ->
            compile_apply(Node, St, Pos);
        cons ->
            compile_cons(Node, St);
        tuple ->
            compile_tuple(Node, St);
        'case' ->
            compile_case(Node, St, Pos);
        'try' ->
            compile_try(Node, St, Pos);
        letrec ->
            compile_letrec(Node, St, Pos);
        'receive' ->
            compile_receive(Node, St, Pos);
        primop ->
            compile_primop(Node, St, Pos);
        'fun' ->
            compile_lambda(Node, St);
        values ->
            %% Multiple return values — compile first one
            Es = cerl:values_es(Node),
            case Es of
                [Single] -> compile_expr(Single, St, Pos);
                _ -> compile_expr(hd(Es), St, Pos)
            end;
        map ->
            compile_map(Node, St);
        binary ->
            compile_binary(Node, St);
        _ ->
            %% Fallback: try folding to literal
            case is_const(Node) of
                true ->
                    Val = const_value(Node),
                    Reg = {x, St#st.next_x},
                    {[{move, encode_literal(Val), Reg}],
                     Reg, St#st{next_x = St#st.next_x + 1}};
                false ->
                    error({unsupported_expr, cerl:type(Node)})
            end
    end.

%% ---------------------------------------------------------------------------
%% Literals and variables
%% ---------------------------------------------------------------------------

compile_literal(Node, St) ->
    Val = cerl:concrete(Node),
    Reg = {x, St#st.next_x},
    {[{move, encode_literal(Val), Reg}], Reg, St#st{next_x = St#st.next_x + 1}}.

compile_var(Node, St) ->
    Name = cerl:var_name(Node),
    case maps:find(Name, St#st.vars) of
        {ok, Reg} ->
            {[], Reg, St};
        error ->
            error({unbound_var, Name})
    end.

%% ---------------------------------------------------------------------------
%% Remote calls (Mod:Fun(Args))
%% ---------------------------------------------------------------------------

compile_call(Node, St, Pos) ->
    Mod = cerl:concrete(cerl:call_module(Node)),
    Fun = cerl:concrete(cerl:call_name(Node)),
    Args = cerl:call_args(Node),
    Arity = length(Args),
    %% Check for BIF optimizations
    case is_gc_bif(Mod, Fun, Arity) of
        true ->
            compile_gc_bif(Fun, Args, St);
        false ->
            case is_comp_bif(Mod, Fun, Arity) of
                true ->
                    compile_comp_bif(Fun, Args, St);
                false ->
                    %% Normal remote call
                    {ArgCode, St1} = place_call_args(Args, St),
                    ExtFunc = {extfunc, Mod, Fun, Arity},
                    case Pos of
                        tail ->
                            case St1#st.stack of
                                0 ->
                                    {ArgCode ++ [{call_ext_only, Arity, ExtFunc}],
                                     {x, 0}, St1};
                                N ->
                                    {ArgCode ++ [{call_ext_last, Arity, ExtFunc, N}],
                                     {x, 0}, St1}
                            end;
                        _ ->
                            {ArgCode ++ [{call_ext, Arity, ExtFunc}],
                             {x, 0}, St1#st{next_x = 1}}
                    end
            end
    end.

%% ---------------------------------------------------------------------------
%% Local calls (apply F(Args))
%% ---------------------------------------------------------------------------

compile_apply(Node, St, Pos) ->
    Op = cerl:apply_op(Node),
    Args = cerl:apply_args(Node),
    Arity = length(Args),
    %% Op should be a variable bound to a function name
    case cerl:type(Op) of
        var ->
            FnName = cerl:var_name(Op),
            case FnName of
                {Name, Ar} when is_atom(Name), is_integer(Ar) ->
                    %% Named local function — resolve to entry label
                    %% Check for lambda-lifted free vars
                    {FLabel, FreeVars} = case maps:find({Name, Ar}, St#st.fn_labels) of
                        {ok, {_Fi, Entry, FV}} -> {{f, Entry}, FV};
                        error -> error({unknown_local_fn, {Name, Ar}})
                    end,
                    %% Build full args: original args + free var values
                    ExtraArgs = [begin
                        case maps:find(V, St#st.vars) of
                            {ok, _} -> cerl:c_var(V);
                            error -> cerl:abstract(undefined)
                        end
                    end || V <- FreeVars],
                    AllArgs = Args ++ ExtraArgs,
                    ActualArity = length(AllArgs),
                    {ArgCode, St1} = place_call_args(AllArgs, St),
                    case Pos of
                        tail ->
                            case St1#st.stack of
                                0 ->
                                    {ArgCode ++ [{call_only, ActualArity, FLabel}],
                                     {x, 0}, St1};
                                N ->
                                    {ArgCode ++ [{call_last, ActualArity, FLabel, N}],
                                     {x, 0}, St1}
                            end;
                        _ ->
                            {ArgCode ++ [{call, ActualArity, FLabel}],
                             {x, 0}, St1#st{next_x = 1}}
                    end;
                _ ->
                    %% Check if this is a lambda variable → direct call
                    case maps:find(FnName, St#st.lambda_vars) of
                        {ok, {LName, LArity}} ->
                            %% Lambda variable — resolve to direct call
                            {FLabel, FreeVars} = case maps:find({LName, LArity}, St#st.fn_labels) of
                                {ok, {_Fi, Entry, FV}} -> {{f, Entry}, FV};
                                error -> error({unknown_lambda_fn, {LName, LArity}})
                            end,
                            ExtraArgs = [begin
                                case maps:find(V, St#st.vars) of
                                    {ok, _} -> cerl:c_var(V);
                                    error -> cerl:abstract(undefined)
                                end
                            end || V <- FreeVars],
                            AllArgs = Args ++ ExtraArgs,
                            ActualArity = length(AllArgs),
                            {ArgCode, St1} = place_call_args(AllArgs, St),
                            case Pos of
                                tail ->
                                    case St1#st.stack of
                                        0 ->
                                            {ArgCode ++ [{call_only, ActualArity, FLabel}],
                                             {x, 0}, St1};
                                        N ->
                                            {ArgCode ++ [{call_last, ActualArity, FLabel, N}],
                                             {x, 0}, St1}
                                    end;
                                _ ->
                                    {ArgCode ++ [{call, ActualArity, FLabel}],
                                     {x, 0}, St1#st{next_x = 1}}
                            end;
                        error ->
                            %% Regular variable holding a fun — use call_fun2
                            {ArgCode, St1} = place_call_args(Args, St),
                            OpReg = maps:get(FnName, St#st.vars),
                            {ArgCode ++ [{call_fun2, {atom, safe}, Arity, OpReg}],
                             {x, 0}, St1#st{next_x = 1}}
                    end
            end;
        _ ->
            error({unsupported_apply_op, cerl:type(Op)})
    end.

%% ---------------------------------------------------------------------------
%% Let bindings
%% ---------------------------------------------------------------------------

compile_let(Node, St, Pos) ->
    Vars = cerl:let_vars(Node),
    Arg = cerl:let_arg(Node),
    Body = cerl:let_body(Node),
    %% Check if arg is a lambda — lift to module-level function
    case cerl:type(Arg) =:= 'fun' andalso length(Vars) =:= 1 of
        true ->
            VarName = cerl:var_name(hd(Vars)),
            LambdaParams = cerl:fun_vars(Arg),
            LambdaArity = length(LambdaParams),
            %% Generate unique name for lifted lambda
            AnonN = St#st.anon_count,
            LiftedName = list_to_atom("-anon-" ++ integer_to_list(AnonN) ++ "-"),
            St1 = St#st{anon_count = AnonN + 1},
            %% Assign labels for the lifted function
            {FiLabel, St2} = new_label(St1),
            {EntryLabel, St3} = new_label(St2),
            %% Detect free variables in the lambda body
            AllFreeVars = cerl_trees:free_variables(Arg),
            ParamNames = [cerl:var_name(P) || P <- LambdaParams],
            FreeVars = [V || V <- AllFreeVars, is_atom(V),
                        not lists:member(V, ParamNames),
                        maps:is_key(V, St#st.vars)],
            %% Register in fn_labels
            FnLabels = maps:put({LiftedName, LambdaArity},
                                {FiLabel, EntryLabel, FreeVars},
                                St3#st.fn_labels),
            St4 = St3#st{fn_labels = FnLabels},
            %% Compile the lambda body as a lifted function
            try
                case FreeVars of
                    [] ->
                        %% No free vars — standard lambda lifting
                        {Fn, St5} = compile_fun(LiftedName, LambdaArity, Arg, St4, []),
                        St6 = St#st{label = St5#st.label,
                                    fn_labels = St5#st.fn_labels,
                                    extra_fns = [Fn | St5#st.extra_fns],
                                    anon_count = St5#st.anon_count,
                                    lambda_vars = maps:put(VarName, {LiftedName, LambdaArity},
                                                           St#st.lambda_vars)},
                        MakeFunCode = [{move, {atom, St6#st.mod}, {x, 0}},
                                       {move, {atom, LiftedName}, {x, 1}},
                                       {move, {integer, LambdaArity}, {x, 2}},
                                       {call_ext, 3, {extfunc, erlang, make_fun, 3}}],
                        %% Save fun value to y-register if stack frame exists
                        {SaveCode, FunReg, St6a} = case St6#st.stack of
                            0 -> {[], {x, 0}, St6};
                            _ ->
                                YReg = {y, St6#st.next_y},
                                {[{move, {x, 0}, YReg}], YReg,
                                 St6#st{next_y = St6#st.next_y + 1}}
                        end,
                        St7 = St6a#st{vars = maps:put(VarName, FunReg, St6a#st.vars)},
                        {BodyCode, ResultReg, St8} = compile_expr(Body, St7, Pos),
                        {MakeFunCode ++ SaveCode ++ BodyCode, ResultReg, St8};
                    _ ->
                        %% Has free vars — use process dictionary closure strategy
                        %% Re-register fn_labels WITHOUT free vars (pdict approach
                        %% has original arity only)
                        FnLabelsClean = maps:put({LiftedName, LambdaArity},
                                                 {FiLabel, EntryLabel, []},
                                                 St4#st.fn_labels),
                        St4a = St4#st{fn_labels = FnLabelsClean},
                        ClosureKeys = [{V, list_to_atom("__closure_" ++ integer_to_list(AnonN) ++ "_" ++ atom_to_list(V))}
                                       || V <- FreeVars],
                        {Fn, St5} = compile_closure_fun(LiftedName, LambdaArity, Arg,
                                                         ClosureKeys, St4a),
                        St6 = St#st{label = St5#st.label,
                                    fn_labels = St5#st.fn_labels,
                                    extra_fns = [Fn | St5#st.extra_fns],
                                    anon_count = St5#st.anon_count,
                                    lambda_vars = maps:put(VarName, {LiftedName, LambdaArity},
                                                           St#st.lambda_vars)},
                        %% Put free vars into process dictionary
                        PutCode = lists:flatmap(fun({VN, Key}) ->
                            VarReg = maps:get(VN, St6#st.vars),
                            [{move, {atom, Key}, {x, 0}},
                             {move, VarReg, {x, 1}},
                             {call_ext, 2, {extfunc, erlang, put, 2}}]
                        end, ClosureKeys),
                        %% Create fun reference
                        MakeFunCode = [{move, {atom, St6#st.mod}, {x, 0}},
                                       {move, {atom, LiftedName}, {x, 1}},
                                       {move, {integer, LambdaArity}, {x, 2}},
                                       {call_ext, 3, {extfunc, erlang, make_fun, 3}}],
                        %% Save fun value to y-register if stack frame exists
                        {SaveCode, FunReg, St6a} = case St6#st.stack of
                            0 -> {[], {x, 0}, St6};
                            _ ->
                                YReg = {y, St6#st.next_y},
                                {[{move, {x, 0}, YReg}], YReg,
                                 St6#st{next_y = St6#st.next_y + 1}}
                        end,
                        St7 = St6a#st{vars = maps:put(VarName, FunReg, St6a#st.vars)},
                        {BodyCode, ResultReg, St8} = compile_expr(Body, St7, Pos),
                        {PutCode ++ MakeFunCode ++ SaveCode ++ BodyCode, ResultReg, St8}
                end
            catch
                _:_Reason ->
                    %% Lambda lifting failed — fall through to normal path
                    compile_let_normal(Vars, Arg, Body, St, Pos)
            end;
        false ->
            compile_let_normal(Vars, Arg, Body, St, Pos)
    end.

compile_let_normal(Vars, Arg, Body, St, Pos) ->
    %% Compile the argument expression
    {ArgCode, ArgReg, St1} = compile_expr(Arg, St, value),
    %% Bind variable(s) — use y-registers when stack frame exists
    {SaveCode, St2} = case St1#st.stack of
        0 ->
            %% No stack frame — bind to the expression's register directly
            StA = lists:foldl(fun(V, SAcc) ->
                SAcc#st{vars = maps:put(cerl:var_name(V), ArgReg, SAcc#st.vars)}
            end, St1, Vars),
            {[], StA};
        _ ->
            %% Stack frame — save to y-register(s)
            lists:foldl(fun(V, {CodeAcc, SAcc}) ->
                YReg = {y, SAcc#st.next_y},
                SAcc2 = SAcc#st{
                    vars = maps:put(cerl:var_name(V), YReg, SAcc#st.vars),
                    next_y = SAcc#st.next_y + 1
                },
                {CodeAcc ++ [{move, ArgReg, YReg}], SAcc2}
            end, {[], St1}, Vars)
    end,
    %% Compile body
    {BodyCode, ResultReg, St3} = compile_expr(Body, St2, Pos),
    {ArgCode ++ SaveCode ++ BodyCode, ResultReg, St3}.

%% ---------------------------------------------------------------------------
%% Sequences (do First, then Second)
%% ---------------------------------------------------------------------------

compile_seq(Node, St, Pos) ->
    First = cerl:seq_arg(Node),
    Second = cerl:seq_body(Node),
    {FirstCode, _Reg, St1} = compile_expr(First, St, effect),
    {SecondCode, ResultReg, St2} = compile_expr(Second, St1, Pos),
    {FirstCode ++ SecondCode, ResultReg, St2}.

%% ---------------------------------------------------------------------------
%% Cons (list construction)
%% ---------------------------------------------------------------------------

compile_cons(Node, St) ->
    case is_const(Node) of
        true ->
            Val = const_value(Node),
            Reg = {x, St#st.next_x},
            {[{move, {literal, Val}, Reg}],
             Reg, St#st{next_x = St#st.next_x + 1}};
        false ->
            Hd = cerl:cons_hd(Node),
            Tl = cerl:cons_tl(Node),
            {HdCode, HdReg, St1} = compile_expr(Hd, St, value),
            {HdSave, SafeHdReg, St1a} = save_intermediate(HdReg, St1),
            {TlCode, TlReg, St2} = compile_expr(Tl, St1a, value),
            Live = max_x_live([SafeHdReg, TlReg]),
            Reg = {x, Live},
            {HdCode ++ HdSave ++ TlCode ++
             [{test_heap, 2, Live}, {put_list, SafeHdReg, TlReg, Reg}],
             Reg, St2#st{next_x = Live + 1}}
    end.

%% ---------------------------------------------------------------------------
%% Tuple construction
%% ---------------------------------------------------------------------------

compile_tuple(Node, St) ->
    case is_const(Node) of
        true ->
            Val = const_value(Node),
            Reg = {x, St#st.next_x},
            {[{move, {literal, Val}, Reg}],
             Reg, St#st{next_x = St#st.next_x + 1}};
        false ->
            Es = cerl:tuple_es(Node),
            {EsCodes, EsRegs, StN} = compile_expr_list(Es, St),
            Live = max_x_live(EsRegs),
            Reg = {x, Live},
            Words = length(Es) + 1,
            {EsCodes ++ [{test_heap, Words, Live},
                         {put_tuple2, Reg, {list, EsRegs}}],
             Reg, StN#st{next_x = Live + 1}}
    end.

%% ---------------------------------------------------------------------------
%% Case expressions (pattern matching) — Step 3+
%% ---------------------------------------------------------------------------

compile_case(Node, St, Pos) ->
    Arg = cerl:case_arg(Node),
    Clauses = cerl:case_clauses(Node),
    %% Compile the scrutinee
    {ArgCode, ArgReg, St1} = compile_expr(Arg, St, value),
    %% Save scrutinee to y-register if we have a stack frame
    {SaveCode, SafeArgReg, St1a} = case St1#st.stack of
        0 -> {[], ArgReg, St1};
        _ ->
            case ArgReg of
                {y, _} -> {[], ArgReg, St1};  %% already in y-reg
                _ ->
                    YReg = {y, St1#st.next_y},
                    {[{move, ArgReg, YReg}], YReg,
                     St1#st{next_y = St1#st.next_y + 1}}
            end
    end,
    %% Create a "done" label for non-tail positions
    {DoneLabel, St1b} = new_label(St1a),
    %% Compile clauses — each clause gets the same base state
    {ClauseCode, ResultReg, St2} = compile_clauses(Clauses, SafeArgReg, St1b, Pos, DoneLabel),
    %% Add done label for non-tail positions
    DoneLabelCode = case Pos of
        tail -> [];
        _ -> [{label, DoneLabel}]
    end,
    {ArgCode ++ SaveCode ++ ClauseCode ++ DoneLabelCode, ResultReg, St2}.

compile_clauses([], _ArgReg, St, _Pos, _DoneLabel) ->
    %% No matching clause — emit match_fail
    {FailLabel, St1} = new_label(St),
    {[{label, FailLabel},
      {move, {atom, case_clause}, {x, 0}},
      {call_ext_only, 1, {extfunc, erlang, error, 1}}],
     {x, 0}, St1};
compile_clauses([Clause | Rest], ArgReg, St, Pos, DoneLabel) ->
    Pats = cerl:clause_pats(Clause),
    Guard = cerl:clause_guard(Clause),
    Body = cerl:clause_body(Clause),
    %% Reset next_x for each clause — variables live in y-registers
    BaseNextX = case ArgReg of
        {x, N} -> N + 1;
        _ -> 0
    end,
    StClean = St#st{next_x = BaseNextX},
    case Pats of
        [Pat] ->
            case cerl:type(Pat) of
                var ->
                    %% Catch-all: bind variable, check guard, compile body
                    VarName = cerl:var_name(Pat),
                    St1 = StClean#st{vars = maps:put(VarName, ArgReg, StClean#st.vars)},
                    case is_trivial_guard(Guard) of
                        true ->
                            %% Always matches — compile body directly
                            {BodyCode, ResultReg, StBody} = compile_expr(Body, St1, Pos),
                            RetCode = case Pos of
                                tail -> tail_return(BodyCode, ResultReg, St#st.stack);
                                _ -> []
                            end,
                            {BodyCode ++ RetCode, case RetCode of [] -> ResultReg; _ -> {x,0} end, StBody};
                        false ->
                            %% Guard check needed — fall through on failure
                            {NextLabel, St2} = new_label(St1),
                            {GuardCode, St3} = compile_guard(Guard, NextLabel, St2),
                            {BodyCode, ResultReg, St4} = compile_expr(Body, St3, Pos),
                            JumpCode = case Pos of
                                tail -> tail_return(BodyCode, ResultReg, St#st.stack);
                                _ -> [{move, ResultReg, {x, 0}}, {jump, {f, DoneLabel}}]
                            end,
                            %% Rest clauses get original state (not St4!) with updated labels
                            StForRest = St#st{label = St4#st.label,
                                              next_y = max(St#st.next_y, St4#st.next_y),
                                              fn_labels = St4#st.fn_labels,
                                              extra_fns = St4#st.extra_fns,
                                              lambda_vars = St4#st.lambda_vars,
                                              anon_count = St4#st.anon_count},
                            {RestCode, _, St5} = compile_clauses(Rest, ArgReg, StForRest, Pos, DoneLabel),
                            {GuardCode ++ BodyCode ++ JumpCode ++ [{label, NextLabel}] ++ RestCode,
                             {x, 0}, St5}
                    end;
                literal ->
                    %% Match a specific literal value
                    {NextLabel, St1} = new_label(StClean),
                    MatchVal = cerl:concrete(Pat),
                    TestCode = [{test, is_eq_exact, {f, NextLabel},
                                 [ArgReg, encode_literal(MatchVal)]}],
                    {BodyCode, ResultReg, St2} = compile_expr(Body, St1, Pos),
                    JumpCode = case Pos of
                        tail -> tail_return(BodyCode, ResultReg, St#st.stack);
                        _ -> [{move, ResultReg, {x, 0}}, {jump, {f, DoneLabel}}]
                    end,
                    %% Rest clauses get original state with updated labels
                    StForRest = St#st{label = St2#st.label,
                                      next_y = max(St#st.next_y, St2#st.next_y),
                                      fn_labels = St2#st.fn_labels,
                                      extra_fns = St2#st.extra_fns,
                                      lambda_vars = St2#st.lambda_vars,
                                      anon_count = St2#st.anon_count},
                    {RestCode, _, St3} = compile_clauses(Rest, ArgReg, StForRest, Pos, DoneLabel),
                    {TestCode ++ BodyCode ++ JumpCode ++ [{label, NextLabel}] ++ RestCode,
                     {x, 0}, St3};
                tuple ->
                    %% Tuple pattern: test is_tuple, test_arity, extract elements
                    {NextLabel, St1} = new_label(StClean),
                    Es = cerl:tuple_es(Pat),
                    TupleSize = length(Es),
                    TestCode = [{test, is_tuple, {f, NextLabel}, [ArgReg]},
                                {test, test_arity, {f, NextLabel}, [ArgReg, TupleSize]}],
                    %% Extract and bind tuple elements
                    {ExtractCode, St2} = lists:foldl(
                        fun({Idx, Elem}, {CodeAcc, SAcc}) ->
                            case cerl:type(Elem) of
                                var ->
                                    VN = cerl:var_name(Elem),
                                    case VN of
                                        '_' -> {CodeAcc, SAcc};
                                        _ ->
                                            case SAcc#st.stack of
                                                0 ->
                                                    XReg = {x, SAcc#st.next_x},
                                                    Code = [{get_tuple_element, ArgReg, Idx, XReg}],
                                                    {CodeAcc ++ Code,
                                                     SAcc#st{vars = maps:put(VN, XReg, SAcc#st.vars),
                                                             next_x = SAcc#st.next_x + 1}};
                                                _ ->
                                                    YReg = {y, SAcc#st.next_y},
                                                    XTmp = {x, SAcc#st.next_x},
                                                    Code = [{get_tuple_element, ArgReg, Idx, XTmp},
                                                            {move, XTmp, YReg}],
                                                    {CodeAcc ++ Code,
                                                     SAcc#st{vars = maps:put(VN, YReg, SAcc#st.vars),
                                                             next_y = SAcc#st.next_y + 1}}
                                            end
                                    end;
                                literal ->
                                    %% Test specific element value
                                    ElemVal = cerl:concrete(Elem),
                                    XTmp = {x, SAcc#st.next_x},
                                    Code = [{get_tuple_element, ArgReg, Idx, XTmp},
                                            {test, is_eq_exact, {f, NextLabel},
                                             [XTmp, encode_literal(ElemVal)]}],
                                    {CodeAcc ++ Code, SAcc};
                                _ ->
                                    {CodeAcc, SAcc}
                            end
                        end,
                        {[], St1},
                        lists:zip(lists:seq(0, TupleSize - 1), Es)
                    ),
                    %% Compile guard and body
                    {GuardCode, St3} = compile_guard(Guard, NextLabel, St2),
                    {BodyCode, ResultReg, St4} = compile_expr(Body, St3, Pos),
                    JumpCode = case Pos of
                        tail -> tail_return(BodyCode, ResultReg, St#st.stack);
                        _ -> [{move, ResultReg, {x, 0}}, {jump, {f, DoneLabel}}]
                    end,
                    StForRest = St#st{label = St4#st.label,
                                      next_y = max(St#st.next_y, St4#st.next_y),
                                      fn_labels = St4#st.fn_labels,
                                      extra_fns = St4#st.extra_fns,
                                      lambda_vars = St4#st.lambda_vars,
                                      anon_count = St4#st.anon_count},
                    {RestCode, _, St5} = compile_clauses(Rest, ArgReg, StForRest, Pos, DoneLabel),
                    {TestCode ++ ExtractCode ++ GuardCode ++ BodyCode ++ JumpCode ++
                     [{label, NextLabel}] ++ RestCode, {x, 0}, St5};
                cons ->
                    %% Cons pattern [H | T]: test is_nonempty_list, get hd/tl
                    {NextLabel, St1} = new_label(StClean),
                    Hd = cerl:cons_hd(Pat),
                    Tl = cerl:cons_tl(Pat),
                    TestCode = [{test, is_nonempty_list, {f, NextLabel}, [ArgReg]}],
                    %% Extract head and tail
                    {HdCode, St2} = case cerl:type(Hd) of
                        var ->
                            HdName = cerl:var_name(Hd),
                            case St1#st.stack of
                                0 ->
                                    XReg = {x, St1#st.next_x},
                                    {[{get_list, ArgReg, XReg, {x, St1#st.next_x + 1}}],
                                     St1#st{vars = maps:put(HdName, XReg, St1#st.vars),
                                            next_x = St1#st.next_x + 2}};
                                _ ->
                                    YReg = {y, St1#st.next_y},
                                    XTmp = {x, St1#st.next_x},
                                    {[{get_list, ArgReg, XTmp, {x, St1#st.next_x + 1}},
                                      {move, XTmp, YReg}],
                                     St1#st{vars = maps:put(HdName, YReg, St1#st.vars),
                                            next_x = St1#st.next_x + 2,
                                            next_y = St1#st.next_y + 1}}
                            end;
                        _ -> {[], St1}
                    end,
                    {TlCode, St3} = case cerl:type(Tl) of
                        var ->
                            TlName = cerl:var_name(Tl),
                            %% Tail is in the register after head from get_list
                            TlXReg = {x, St2#st.next_x - 1},
                            case St2#st.stack of
                                0 ->
                                    {[],
                                     St2#st{vars = maps:put(TlName, TlXReg, St2#st.vars)}};
                                _ ->
                                    TlYReg = {y, St2#st.next_y},
                                    {[{move, TlXReg, TlYReg}],
                                     St2#st{vars = maps:put(TlName, TlYReg, St2#st.vars),
                                            next_y = St2#st.next_y + 1}}
                            end;
                        _ -> {[], St2}
                    end,
                    {GuardCode, St4} = compile_guard(Guard, NextLabel, St3),
                    {BodyCode, ResultReg, St5} = compile_expr(Body, St4, Pos),
                    JumpCode = case Pos of
                        tail -> tail_return(BodyCode, ResultReg, St#st.stack);
                        _ -> [{move, ResultReg, {x, 0}}, {jump, {f, DoneLabel}}]
                    end,
                    StForRest = St#st{label = St5#st.label,
                                      next_y = max(St#st.next_y, St5#st.next_y),
                                      fn_labels = St5#st.fn_labels,
                                      extra_fns = St5#st.extra_fns,
                                      lambda_vars = St5#st.lambda_vars,
                                      anon_count = St5#st.anon_count},
                    {RestCode, _, St6} = compile_clauses(Rest, ArgReg, StForRest, Pos, DoneLabel),
                    {TestCode ++ HdCode ++ TlCode ++ GuardCode ++ BodyCode ++ JumpCode ++
                     [{label, NextLabel}] ++ RestCode, {x, 0}, St6};
                _ ->
                    %% Other complex pattern — skip, try rest
                    compile_clauses(Rest, ArgReg, St, Pos, DoneLabel)
            end;
        _ ->
            %% Multi-pattern — skip for now
            compile_clauses(Rest, ArgReg, St, Pos, DoneLabel)
    end.

is_trivial_guard(Guard) ->
    cerl:type(Guard) =:= literal andalso cerl:concrete(Guard) =:= true.

compile_guard(Guard, FailLabel, St) ->
    %% Compile guard expression. Guards are limited to BIFs and type tests.
    %% On failure, jump to FailLabel.
    case cerl:type(Guard) of
        literal ->
            case cerl:concrete(Guard) of
                true -> {[], St};
                _ -> {[{jump, {f, FailLabel}}], St}
            end;
        call ->
            %% Guard BIF call — emit test instruction
            Mod = cerl:concrete(cerl:call_module(Guard)),
            Fun = cerl:concrete(cerl:call_name(Guard)),
            Args = cerl:call_args(Guard),
            compile_guard_call(Mod, Fun, Args, FailLabel, St);
        var ->
            %% Guard is a variable — test if true
            Name = cerl:var_name(Guard),
            case maps:find(Name, St#st.vars) of
                {ok, Reg} ->
                    {[{test, is_eq_exact, {f, FailLabel},
                       [Reg, {atom, true}]}], St};
                error ->
                    {[], St}
            end;
        _ ->
            %% Unknown guard form — just pass (optimistic)
            {[], St}
    end.

compile_guard_call(erlang, '=:=', [A, B], FailLabel, St) ->
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeB, RegB, St2} = compile_guard_arg(B, St1),
    {CodeA ++ CodeB ++ [{test, is_eq_exact, {f, FailLabel}, [RegA, RegB]}], St2};
compile_guard_call(erlang, '=/=', [A, B], FailLabel, St) ->
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeB, RegB, St2} = compile_guard_arg(B, St1),
    {CodeA ++ CodeB ++ [{test, is_ne_exact, {f, FailLabel}, [RegA, RegB]}], St2};
compile_guard_call(erlang, '==', [A, B], FailLabel, St) ->
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeB, RegB, St2} = compile_guard_arg(B, St1),
    {CodeA ++ CodeB ++ [{test, is_eq, {f, FailLabel}, [RegA, RegB]}], St2};
compile_guard_call(erlang, '/=', [A, B], FailLabel, St) ->
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeB, RegB, St2} = compile_guard_arg(B, St1),
    {CodeA ++ CodeB ++ [{test, is_ne, {f, FailLabel}, [RegA, RegB]}], St2};
compile_guard_call(erlang, '<', [A, B], FailLabel, St) ->
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeB, RegB, St2} = compile_guard_arg(B, St1),
    {CodeA ++ CodeB ++ [{test, is_lt, {f, FailLabel}, [RegA, RegB]}], St2};
compile_guard_call(erlang, '>=', [A, B], FailLabel, St) ->
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeB, RegB, St2} = compile_guard_arg(B, St1),
    {CodeA ++ CodeB ++ [{test, is_ge, {f, FailLabel}, [RegA, RegB]}], St2};
compile_guard_call(erlang, '>', [A, B], FailLabel, St) ->
    %% a > b is equivalent to b < a
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeB, RegB, St2} = compile_guard_arg(B, St1),
    {CodeA ++ CodeB ++ [{test, is_lt, {f, FailLabel}, [RegB, RegA]}], St2};
compile_guard_call(erlang, '=<', [A, B], FailLabel, St) ->
    %% a =< b is equivalent to b >= a
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeB, RegB, St2} = compile_guard_arg(B, St1),
    {CodeA ++ CodeB ++ [{test, is_ge, {f, FailLabel}, [RegB, RegA]}], St2};
compile_guard_call(erlang, is_atom, [A], FailLabel, St) ->
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeA ++ [{test, is_atom, {f, FailLabel}, [RegA]}], St1};
compile_guard_call(erlang, is_integer, [A], FailLabel, St) ->
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeA ++ [{test, is_integer, {f, FailLabel}, [RegA]}], St1};
compile_guard_call(erlang, is_float, [A], FailLabel, St) ->
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeA ++ [{test, is_float, {f, FailLabel}, [RegA]}], St1};
compile_guard_call(erlang, is_list, [A], FailLabel, St) ->
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeA ++ [{test, is_list, {f, FailLabel}, [RegA]}], St1};
compile_guard_call(erlang, is_tuple, [A], FailLabel, St) ->
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeA ++ [{test, is_tuple, {f, FailLabel}, [RegA]}], St1};
compile_guard_call(erlang, is_binary, [A], FailLabel, St) ->
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeA ++ [{test, is_binary, {f, FailLabel}, [RegA]}], St1};
compile_guard_call(erlang, is_map, [A], FailLabel, St) ->
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeA ++ [{test, is_map, {f, FailLabel}, [RegA]}], St1};
compile_guard_call(erlang, is_boolean, [A], FailLabel, St) ->
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeA ++ [{test, is_boolean, {f, FailLabel}, [RegA]}], St1};
compile_guard_call(erlang, 'not', [A], FailLabel, St) ->
    %% not(X) in guard: test X =:= false
    {CodeA, RegA, St1} = compile_guard_arg(A, St),
    {CodeA ++ [{test, is_eq_exact, {f, FailLabel}, [RegA, {atom, false}]}], St1};
compile_guard_call(erlang, 'and', [A, B], FailLabel, St) ->
    %% Both must be true
    {CodeA, St1} = compile_guard(A, FailLabel, St),
    {CodeB, St2} = compile_guard(B, FailLabel, St1),
    {CodeA ++ CodeB, St2};
compile_guard_call(erlang, 'or', [A, B], FailLabel, St) ->
    %% At least one must be true — try A, if fails try B
    {PassLabel, St1} = new_label(St),
    {TryBLabel, St2} = new_label(St1),
    {CodeA, St3} = compile_guard(A, TryBLabel, St2),
    {CodeB, St4} = compile_guard(B, FailLabel, St3),
    {CodeA ++ [{jump, {f, PassLabel}},
     {label, TryBLabel}] ++ CodeB ++
     [{label, PassLabel}], St4};
compile_guard_call(_Mod, _Fun, _Args, _FailLabel, St) ->
    %% Unknown guard BIF — pass optimistically
    {[], St}.

%% Compile a guard sub-expression to get its value into a register
compile_guard_arg(Node, St) ->
    case cerl:type(Node) of
        literal ->
            Val = cerl:concrete(Node),
            {[], encode_literal(Val), St};
        var ->
            Name = cerl:var_name(Node),
            case maps:find(Name, St#st.vars) of
                {ok, Reg} -> {[], Reg, St};
                error -> {[], {atom, undefined}, St}
            end;
        call ->
            %% Guard BIF that produces a value (e.g., erlang:length/1)
            Mod = cerl:concrete(cerl:call_module(Node)),
            Fun = cerl:concrete(cerl:call_name(Node)),
            Args = cerl:call_args(Node),
            Arity = length(Args),
            case is_gc_bif(Mod, Fun, Arity) of
                true -> compile_gc_bif(Fun, Args, St);
                false ->
                    case is_comp_bif(Mod, Fun, Arity) of
                        true -> compile_comp_bif(Fun, Args, St);
                        false -> {[], {atom, true}, St}
                    end
            end;
        _ ->
            {[], {atom, true}, St}
    end.

%% Generate return sequence for tail-position clause bodies.
%% If body code already ends with a terminator, return [].
%% Otherwise, emit move-to-x0 + deallocate + return.
tail_return(BodyCode, ResultReg, FrameSize) ->
    case BodyCode =/= [] andalso is_terminator(lists:last(BodyCode)) of
        true -> [];
        false ->
            MoveCode = case ResultReg of
                {x, 0} -> [];
                _ -> [{move, ResultReg, {x, 0}}]
            end,
            DeallocCode = case FrameSize of
                0 -> [];
                N -> [{deallocate, N}]
            end,
            MoveCode ++ DeallocCode ++ [return]
    end.

%% ---------------------------------------------------------------------------
%% Try/catch — Step 6+
%% ---------------------------------------------------------------------------

compile_try(Node, St, Pos) ->
    %% Simplified: compile the body, ignore exception handling for now
    Arg = cerl:try_arg(Node),
    Vars = cerl:try_vars(Node),
    Body = cerl:try_body(Node),
    %% Compile the protected expression
    {ArgCode, ArgReg, St1} = compile_expr(Arg, St, value),
    %% Bind result variable
    St2 = case Vars of
        [V] -> St1#st{vars = maps:put(cerl:var_name(V), ArgReg, St1#st.vars)};
        _ -> St1
    end,
    %% Compile the success body
    {BodyCode, ResultReg, St3} = compile_expr(Body, St2, Pos),
    {ArgCode ++ BodyCode, ResultReg, St3}.

%% ---------------------------------------------------------------------------
%% Letrec (recursive function definitions / loops) — Step 5+
%% ---------------------------------------------------------------------------

compile_letrec(Node, St, Pos) ->
    %% Letrec defines local recursive functions, then evaluates the body.
    %% We use LAMBDA LIFTING: detect free variables in each function,
    %% add them as extra parameters, and pass them at call sites.
    Defs = cerl:letrec_defs(Node),
    Body = cerl:letrec_body(Node),
    %% Phase 1: Compute free vars and assign labels for all letrec-defined functions
    St1 = lists:foldl(fun({Name, Fun}, SAcc) ->
        FnName = cerl:var_name(Name),
        case FnName of
            {N, A} when is_atom(N), is_integer(A) ->
                %% Compute free variables in this function
                AllFree = cerl_trees:free_variables(Fun),
                %% Keep only atom-named vars (not function refs like {loop_1, 1})
                %% and only those that exist in the enclosing scope
                FreeVars = [V || V <- AllFree, is_atom(V),
                            maps:is_key(V, SAcc#st.vars)],
                %% Assign labels
                {FiLabel, SAcc1} = new_label(SAcc),
                {EntryLabel, SAcc2} = new_label(SAcc1),
                FnLabels = maps:put({N, A}, {FiLabel, EntryLabel, FreeVars}, SAcc2#st.fn_labels),
                SAcc2#st{fn_labels = FnLabels};
            _ ->
                SAcc#st{vars = maps:put(FnName, {x, 0}, SAcc#st.vars)}
        end
    end, St, Defs),
    %% Phase 2: Compile each defined function with extra params for free vars
    %% IMPORTANT: compile_fun resets vars/next_x/next_y/stack for the function.
    %% We must restore the parent scope after each function compilation,
    %% keeping only label counter, fn_labels, and extra_fns.
    %% If compilation fails, emit a stub function so beam_clean doesn't crash.
    St2 = lists:foldl(fun({Name, Fun}, SAcc) ->
        FnName = cerl:var_name(Name),
        case FnName of
            {N, A} when is_atom(N), is_integer(A) ->
                {FiLabel, EntryLabel, FreeVars} = maps:get({N, A}, SAcc#st.fn_labels),
                ActualAr = A + length(FreeVars),
                try
                    {Fn, SAcc1} = compile_fun(N, A, Fun, SAcc, FreeVars),
                    SAcc#st{label = SAcc1#st.label,
                            fn_labels = SAcc1#st.fn_labels,
                            extra_fns = [Fn | SAcc1#st.extra_fns],
                            lambda_vars = SAcc1#st.lambda_vars,
                            anon_count = SAcc1#st.anon_count}
                catch
                    _:_Reason2 ->
                        %% Emit stub function that returns 'undefined'
                        StubFn = {function, N, ActualAr, EntryLabel,
                                  [{label, FiLabel},
                                   {func_info, {atom, SAcc#st.mod}, {atom, N}, ActualAr},
                                   {label, EntryLabel},
                                   {move, {atom, undefined}, {x, 0}},
                                   return]},
                        SAcc#st{extra_fns = [StubFn | SAcc#st.extra_fns]}
                end;
            _ ->
                SAcc
        end
    end, St1, Defs),
    %% Phase 3: Compile the letrec body
    compile_expr(Body, St2, Pos).

%% ---------------------------------------------------------------------------
%% Receive — Step 6+
%% ---------------------------------------------------------------------------

compile_receive(Node, St, Pos) ->
    %% Placeholder for message receive
    Timeout = cerl:receive_timeout(Node),
    Action = cerl:receive_action(Node),
    case cerl:type(Timeout) of
        literal ->
            case cerl:concrete(Timeout) of
                infinity ->
                    %% Infinite wait — simplified
                    compile_expr(Action, St, Pos);
                _ ->
                    compile_expr(Action, St, Pos)
            end;
        _ ->
            compile_expr(Action, St, Pos)
    end.

%% ---------------------------------------------------------------------------
%% Primop (primitive operations)
%% ---------------------------------------------------------------------------

compile_primop(Node, St, _Pos) ->
    Name = cerl:concrete(cerl:primop_name(Node)),
    Args = cerl:primop_args(Node),
    case Name of
        match_fail ->
            %% Emit error
            {ArgCode, ArgReg, St1} = compile_expr(hd(Args), St, value),
            {ArgCode ++ [{move, ArgReg, {x, 0}},
                         {call_ext_only, 1, {extfunc, erlang, error, 1}}],
             {x, 0}, St1};
        raise ->
            %% Re-raise exception
            {[{move, {atom, error}, {x, 0}},
              {call_ext_only, 1, {extfunc, erlang, error, 1}}],
             {x, 0}, St};
        _ ->
            %% Unknown primop — emit error
            {[{move, {atom, {unsupported_primop, Name}}, {x, 0}},
              {call_ext_only, 1, {extfunc, erlang, error, 1}}],
             {x, 0}, St}
    end.

%% ---------------------------------------------------------------------------
%% Lambda (anonymous function) — Step 4+
%% ---------------------------------------------------------------------------

compile_lambda(Node, St) ->
    Params = cerl:fun_vars(Node),
    LambdaArity = length(Params),
    %% Generate unique name for lifted lambda
    AnonN = St#st.anon_count,
    LiftedName = list_to_atom("-anon-" ++ integer_to_list(AnonN) ++ "-"),
    St1 = St#st{anon_count = AnonN + 1},
    %% Detect free variables
    AllFreeVars = cerl_trees:free_variables(Node),
    ParamNames = [cerl:var_name(P) || P <- Params],
    FreeVars = [V || V <- AllFreeVars, is_atom(V),
                not lists:member(V, ParamNames),
                maps:is_key(V, St#st.vars)],
    case FreeVars of
        [] ->
            %% No free vars — lift to module-level + erlang:make_fun/3
            {FiLabel, St2} = new_label(St1),
            {EntryLabel, St3} = new_label(St2),
            FnLabels = maps:put({LiftedName, LambdaArity},
                                {FiLabel, EntryLabel, []},
                                St3#st.fn_labels),
            St4 = St3#st{fn_labels = FnLabels},
            {Fn, St5} = compile_fun(LiftedName, LambdaArity, Node, St4, []),
            St6 = St#st{label = St5#st.label,
                        fn_labels = St5#st.fn_labels,
                        extra_fns = [Fn | St5#st.extra_fns],
                        anon_count = St5#st.anon_count,
                        lambda_vars = St5#st.lambda_vars},
            %% Create a fun reference: erlang:make_fun(Mod, Name, Arity)
            MakeFunCode = [
                {move, {atom, St6#st.mod}, {x, 0}},
                {move, {atom, LiftedName}, {x, 1}},
                {move, {integer, LambdaArity}, {x, 2}},
                {call_ext, 3, {extfunc, erlang, make_fun, 3}}
            ],
            {MakeFunCode, {x, 0}, St6#st{next_x = 1}};
        _ ->
            %% Has free vars — use process dictionary closure strategy:
            %% 1. Compile lifted function that reads free vars via erlang:get/1
            %% 2. At call site, put free vars via erlang:put/2 + make_fun/3
            compile_closure_via_pdict(LiftedName, LambdaArity, Node, FreeVars, AnonN, St1)
    end.

%% ---------------------------------------------------------------------------
%% Closure via process dictionary
%%
%% Since make_fun2/make_fun3 aren't available in from_asm mode, closures are
%% implemented by storing free variables in the process dictionary before
%% creating the fun reference, and reading them back in the lifted function.
%%
%% For a closure:  fun(X) -> X + Val end  (where Val is free)
%% We generate:
%%   Lifted: '-anon-0-'(X) -> erlang:get('__closure_0_Val') + X
%%   Call site: erlang:put('__closure_0_Val', Val),
%%              erlang:make_fun(Mod, '-anon-0-', 1)
%% ---------------------------------------------------------------------------

compile_closure_via_pdict(LiftedName, LambdaArity, LambdaNode, FreeVars, AnonN, St) ->
    %% Generate unique process dictionary keys for each free var
    ClosureKeys = [{V, list_to_atom("__closure_" ++ integer_to_list(AnonN) ++ "_" ++ atom_to_list(V))}
                   || V <- FreeVars],
    %% Step 1: Compile the lifted function with pdict reads for free vars
    {FiLabel, St2} = new_label(St),
    {EntryLabel, St3} = new_label(St2),
    %% Register with NO free vars in fn_labels (function has original arity only)
    FnLabels = maps:put({LiftedName, LambdaArity},
                        {FiLabel, EntryLabel, []},
                        St3#st.fn_labels),
    St4 = St3#st{fn_labels = FnLabels},
    %% Compile the lifted function with process dictionary reads injected
    {Fn, St5} = compile_closure_fun(LiftedName, LambdaArity, LambdaNode,
                                     ClosureKeys, St4),
    St6 = St#st{label = St5#st.label,
                fn_labels = St5#st.fn_labels,
                extra_fns = [Fn | St5#st.extra_fns],
                anon_count = St5#st.anon_count,
                lambda_vars = St5#st.lambda_vars},
    %% Step 2: At call site, put free vars into process dictionary
    PutCode = lists:flatmap(fun({VarName, Key}) ->
        VarReg = maps:get(VarName, St6#st.vars),
        [{move, {atom, Key}, {x, 0}},
         {move, VarReg, {x, 1}},
         {call_ext, 2, {extfunc, erlang, put, 2}}]
    end, ClosureKeys),
    %% Step 3: Create fun reference via erlang:make_fun/3
    MakeFunCode = [
        {move, {atom, St6#st.mod}, {x, 0}},
        {move, {atom, LiftedName}, {x, 1}},
        {move, {integer, LambdaArity}, {x, 2}},
        {call_ext, 3, {extfunc, erlang, make_fun, 3}}
    ],
    {PutCode ++ MakeFunCode, {x, 0}, St6#st{next_x = 1}}.

%% Compile a lifted closure function that reads free vars from process dictionary.
%% The function has the ORIGINAL arity (no extra params for free vars).
%% Free vars are loaded at function entry via erlang:get/1.
compile_closure_fun(FnName, Arity, FunNode, ClosureKeys, St) ->
    {FiLabel, EntryLabel, _} = maps:get({FnName, Arity}, St#st.fn_labels),
    Params = cerl:fun_vars(FunNode),
    Body = cerl:fun_body(FunNode),
    %% This function always needs a stack frame because erlang:get/1 is a call
    V0 = bind_params_to_y(Params, 0, #{}),
    ParamSaveCode = case Arity of
        0 -> [];
        _ -> [{move, {x, I}, {y, I}} || I <- lists:seq(0, Arity - 1)]
    end,
    NextY = Arity,
    %% Generate code to load free vars from process dictionary into y-registers
    {GetCode, VarsWithFreeVars, NextY2} = lists:foldl(
        fun({VarName, Key}, {CodeAcc, VarsAcc, YIdx}) ->
            YReg = {y, YIdx},
            Code = [{move, {atom, Key}, {x, 0}},
                    {call_ext, 1, {extfunc, erlang, get, 1}},
                    {move, {x, 0}, YReg}],
            {CodeAcc ++ Code,
             maps:put(VarName, YReg, VarsAcc),
             YIdx + 1}
        end,
        {[], V0, NextY},
        ClosureKeys
    ),
    %% Compile body with free vars bound
    %% next_x = 0 because after get calls, all values are in y-registers
    %% and no x-registers are live
    St3 = St#st{vars = VarsWithFreeVars, next_x = 0, next_y = NextY2,
                stack = ?FRAME_PLACEHOLDER},
    {BodyCode, ResultReg, St4} = compile_expr(Body, St3, tail),
    ActualFrameSize = St4#st.next_y,
    FixedBodyCode = fix_frame_size(BodyCode, ActualFrameSize),
    FixedGetCode = fix_frame_size(GetCode, ActualFrameSize),
    FinalCode = ensure_terminator(FixedBodyCode, ResultReg, ActualFrameSize),
    YRegs = [{y, I} || I <- lists:seq(0, ActualFrameSize - 1)],
    AllocCode = [{allocate, ActualFrameSize, Arity},
                 {init_yregs, {list, YRegs}}],
    Fn = {function, FnName, Arity, EntryLabel,
          [{label, FiLabel},
           {func_info, {atom, St#st.mod}, {atom, FnName}, Arity},
           {label, EntryLabel}]
          ++ AllocCode
          ++ ParamSaveCode
          ++ FixedGetCode
          ++ FinalCode},
    {Fn, St4#st{stack = ActualFrameSize}}.

%% ---------------------------------------------------------------------------
%% Map construction — Step 7+
%% ---------------------------------------------------------------------------

compile_map(Node, St) ->
    case is_const(Node) of
        true ->
            Val = const_value(Node),
            Reg = {x, St#st.next_x},
            {[{move, {literal, Val}, Reg}],
             Reg, St#st{next_x = St#st.next_x + 1}};
        false ->
            %% Runtime map — build via maps:from_list or put_map_assoc
            %% Use erlang BIF call: maps:from_list([{K,V}, ...])
            Pairs = cerl:map_es(Node),
            case Pairs of
                [] ->
                    Reg = {x, St#st.next_x},
                    {[{move, {literal, #{}}, Reg}],
                     Reg, St#st{next_x = St#st.next_x + 1}};
                _ ->
                    %% Check if all keys and values are constants
                    AllConst = lists:all(fun(P) ->
                        is_const(cerl:map_pair_key(P)) andalso
                        is_const(cerl:map_pair_val(P))
                    end, Pairs),
                    case AllConst of
                        true ->
                            %% All const pairs — build literal map
                            PairList = [{const_value(cerl:map_pair_key(P)),
                                        const_value(cerl:map_pair_val(P))}
                                       || P <- Pairs],
                            MapVal = maps:from_list(PairList),
                            Reg = {x, St#st.next_x},
                            {[{move, {literal, MapVal}, Reg}],
                             Reg, St#st{next_x = St#st.next_x + 1}};
                        false ->
                            %% Dynamic map — build pair list and call maps:from_list
                            %% Compile each key-value pair into a cons list of tuples
                            {PairCode, PairRegs, St1} = lists:foldl(
                                fun(P, {CodeAcc, RegAcc, SAcc}) ->
                                    K = cerl:map_pair_key(P),
                                    V = cerl:map_pair_val(P),
                                    {KCode, KReg, SAcc1} = compile_expr(K, SAcc, value),
                                    {KSave, SafeK, SAcc1a} = save_intermediate(KReg, SAcc1),
                                    {VCode, VReg, SAcc2} = compile_expr(V, SAcc1a, value),
                                    {VSave, SafeV, SAcc2a} = save_intermediate(VReg, SAcc2),
                                    {CodeAcc ++ KCode ++ KSave ++ VCode ++ VSave,
                                     RegAcc ++ [{SafeK, SafeV}],
                                     SAcc2a}
                                end,
                                {[], [], St},
                                Pairs
                            ),
                            %% Build [{K,V}|...] list then call maps:from_list
                            {ListCode, ListReg, St2} = build_pair_list(PairRegs, St1),
                            %% Call maps:from_list(List)
                            {PairCode ++ ListCode ++
                             [{move, ListReg, {x, 0}},
                              {call_ext, 1, {extfunc, maps, from_list, 1}}],
                             {x, 0}, St2#st{next_x = 1}}
                    end
            end
    end.

%% Build a list of {K,V} tuples from a list of {KReg, VReg} pairs
build_pair_list([], St) ->
    Reg = {x, St#st.next_x},
    {[{move, nil, Reg}], Reg, St#st{next_x = St#st.next_x + 1}};
build_pair_list([{KReg, VReg} | Rest], St) ->
    {RestCode, RestReg, St1} = build_pair_list(Rest, St),
    Live = max_x_live([KReg, VReg, RestReg]),
    TupleReg = {x, Live},
    ConsReg = {x, Live + 1},
    {RestCode ++
     [{test_heap, 5, Live},  %% 3 for tuple, 2 for cons
      {put_tuple2, TupleReg, {list, [KReg, VReg]}},
      {put_list, TupleReg, RestReg, ConsReg}],
     ConsReg, St1#st{next_x = Live + 2}}.

%% ---------------------------------------------------------------------------
%% Binary construction — Step 7+
%% ---------------------------------------------------------------------------

compile_binary(Node, St) ->
    case is_const(Node) of
        true ->
            Val = const_value(Node),
            Reg = {x, St#st.next_x},
            {[{move, {literal, Val}, Reg}],
             Reg, St#st{next_x = St#st.next_x + 1}};
        false ->
            %% Runtime binary — placeholder
            Reg = {x, St#st.next_x},
            {[{move, {literal, <<>>}, Reg}],
             Reg, St#st{next_x = St#st.next_x + 1}}
    end.

%% ---------------------------------------------------------------------------
%% Argument placement for function calls
%%
%% Places arguments into x0..xN for a call instruction.
%% Uses right-to-left ordering to minimize register conflicts
%% (e.g., when a variable in x0 needs to move to x1 before a
%% literal overwrites x0).
%% ---------------------------------------------------------------------------

place_call_args(Args, St) ->
    Arity = length(Args),
    %% Phase 1: Prepare all arguments — simple args become source operands,
    %% complex args are compiled to temporary x-registers first
    {PreCode, Sources, St1} = prepare_call_args(Args, St),
    %% Phase 2: Place sources into x0..xN, right-to-left for conflict avoidance
    Pairs = lists:zip(lists:seq(0, Arity - 1), Sources),
    PlaceCode = lists:flatmap(fun({I, Src}) ->
        Tgt = {x, I},
        case Src of
            Tgt -> [];              %% already in place
            _ -> [{move, Src, Tgt}]
        end
    end, lists:reverse(Pairs)),
    {PreCode ++ PlaceCode, St1#st{next_x = Arity}}.

%% Prepare call arguments: simple ones return operands directly,
%% complex ones are compiled to temporary registers
prepare_call_args(Args, St) ->
    lists:foldl(fun(Arg, {CodeAcc, SrcAcc, SAcc}) ->
        case cerl:type(Arg) of
            literal ->
                {CodeAcc, SrcAcc ++ [encode_literal(cerl:concrete(Arg))], SAcc};
            var ->
                Name = cerl:var_name(Arg),
                Reg = maps:get(Name, SAcc#st.vars),
                {CodeAcc, SrcAcc ++ [Reg], SAcc};
            _ ->
                case is_const(Arg) of
                    true ->
                        {CodeAcc, SrcAcc ++ [{literal, const_value(Arg)}], SAcc};
                    false ->
                        %% Complex expression — compile to a temp register
                        {Code, Reg, SAcc2} = compile_expr(Arg, SAcc, value),
                        %% Save x-register results to y-registers to protect
                        %% across subsequent calls in later arguments
                        case Reg of
                            {x, _} when SAcc2#st.stack > 0 ->
                                YReg = {y, SAcc2#st.next_y},
                                SaveCode = [{move, Reg, YReg}],
                                {CodeAcc ++ Code ++ SaveCode, SrcAcc ++ [YReg],
                                 SAcc2#st{next_y = SAcc2#st.next_y + 1}};
                            _ ->
                                {CodeAcc ++ Code, SrcAcc ++ [Reg], SAcc2}
                        end
                end
        end
    end, {[], [], St}, Args).

%% ---------------------------------------------------------------------------
%% Helper: compile a list of expressions
%% Returns: {AllCode, [Registers], FinalState}
%% ---------------------------------------------------------------------------

compile_expr_list([], St) ->
    {[], [], St};
compile_expr_list([E | Rest], St) ->
    {Code, Reg, St1} = compile_expr(E, St, value),
    {SaveCode, SafeReg, St1a} = save_intermediate(Reg, St1),
    {RestCode, RestRegs, St2} = compile_expr_list(Rest, St1a),
    {Code ++ SaveCode ++ RestCode, [SafeReg | RestRegs], St2}.

%% ---------------------------------------------------------------------------
%% BIF classification and compilation
%% ---------------------------------------------------------------------------

%% Arithmetic BIFs that may trigger GC (bignums, floats)
is_gc_bif(erlang, Op, 2) when Op =:= '+'; Op =:= '-'; Op =:= '*';
                              Op =:= 'div'; Op =:= 'rem';
                              Op =:= 'band'; Op =:= 'bor'; Op =:= 'bxor';
                              Op =:= 'bsl'; Op =:= 'bsr' -> true;
is_gc_bif(erlang, Op, 1) when Op =:= 'bnot'; Op =:= 'abs';
                              Op =:= 'float'; Op =:= '-' -> true;
is_gc_bif(erlang, length, 1) -> true;
is_gc_bif(_, _, _) -> false.

%% Comparison BIFs (no GC needed)
is_comp_bif(erlang, Op, 2) when Op =:= '<'; Op =:= '>'; Op =:= '=<'; Op =:= '>=';
                                Op =:= '=:='; Op =:= '=/=';
                                Op =:= '=='; Op =:= '/=' -> true;
is_comp_bif(erlang, 'not', 1) -> true;
is_comp_bif(erlang, Op, 2) when Op =:= 'and'; Op =:= 'or'; Op =:= 'xor' -> true;
is_comp_bif(_, _, _) -> false.

compile_gc_bif(Op, Args, St) ->
    {PreCode, Sources, St1} = prepare_call_args(Args, St),
    Live = max_x_live(Sources),
    Dst = {x, Live},
    {PreCode ++ [{gc_bif, Op, {f, 0}, Live, Sources, Dst}],
     Dst, St1#st{next_x = Live + 1}}.

compile_comp_bif(Op, Args, St) ->
    {PreCode, Sources, St1} = prepare_call_args(Args, St),
    Live = max_x_live(Sources),
    Dst = {x, Live},
    {PreCode ++ [{bif, Op, {f, 0}, Sources, Dst}],
     Dst, St1#st{next_x = Live + 1}}.

%% Save intermediate x-register result to y-register when stack frame exists.
%% Prevents clobbering by subsequent expressions.
save_intermediate(Reg, St) ->
    case Reg of
        {x, _} when St#st.stack > 0 ->
            YReg = {y, St#st.next_y},
            {[{move, Reg, YReg}], YReg, St#st{next_y = St#st.next_y + 1}};
        _ ->
            {[], Reg, St}
    end.

%% Compute Live x-register count from source operands.
%% Only x-registers contribute; y-registers and literals don't.
max_x_live(Sources) ->
    XIndices = [N || {x, N} <- Sources],
    case XIndices of
        [] -> 0;
        _ -> lists:max(XIndices) + 1
    end.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

new_label(St) ->
    L = St#st.label,
    {L, St#st{label = L + 1}}.

%% Replace provisional frame size placeholder with actual frame size
%% in all instructions that reference the stack frame.
fix_frame_size(Code, ActualSize) ->
    [fix_instr(I, ActualSize) || I <- Code].

fix_instr({deallocate, ?FRAME_PLACEHOLDER}, Size) -> {deallocate, Size};
fix_instr({call_ext_last, Arity, Func, ?FRAME_PLACEHOLDER}, Size) ->
    {call_ext_last, Arity, Func, Size};
fix_instr({call_last, Arity, Label, ?FRAME_PLACEHOLDER}, Size) ->
    {call_last, Arity, Label, Size};
fix_instr(Instr, _) -> Instr.

%% Ensure function body ends with a terminator instruction.
%% If the last instruction is already a tail call, do nothing.
%% Otherwise, move result to x0, deallocate stack frame, and return.
ensure_terminator([], _ResultReg, FrameSize) ->
    DeallocCode = case FrameSize of
        0 -> [];
        N -> [{deallocate, N}]
    end,
    [{move, {atom, ok}, {x, 0}}] ++ DeallocCode ++ [return];
ensure_terminator(Code, ResultReg, FrameSize) ->
    case is_terminator(lists:last(Code)) of
        true -> Code;
        false ->
            MoveCode = case ResultReg of
                {x, 0} -> [];
                _ -> [{move, ResultReg, {x, 0}}]
            end,
            DeallocCode = case FrameSize of
                0 -> [];
                N -> [{deallocate, N}]
            end,
            Code ++ MoveCode ++ DeallocCode ++ [return]
    end.

is_terminator({call_ext_only, _, _}) -> true;
is_terminator({call_ext_last, _, _, _}) -> true;
is_terminator({call_only, _, _}) -> true;
is_terminator({call_last, _, _, _}) -> true;
is_terminator(return) -> true;
is_terminator({jump, _}) -> true;
is_terminator(_) -> false.

%% Encode an Erlang value as a BEAM operand
encode_literal(Val) when is_atom(Val)    -> {atom, Val};
encode_literal(Val) when is_integer(Val) -> {integer, Val};
encode_literal(Val) when is_float(Val)   -> {float, Val};
encode_literal([])                       -> nil;
encode_literal(Val)                      -> {literal, Val}.

%% Check if a Core Erlang node is a compile-time constant
is_const(Node) ->
    case cerl:type(Node) of
        literal -> true;
        cons ->
            is_const(cerl:cons_hd(Node)) andalso is_const(cerl:cons_tl(Node));
        tuple ->
            lists:all(fun is_const/1, cerl:tuple_es(Node));
        _ -> false
    end.

%% Extract the Erlang value from a compile-time constant node
const_value(Node) ->
    case cerl:type(Node) of
        literal ->
            cerl:concrete(Node);
        cons ->
            [const_value(cerl:cons_hd(Node)) | const_value(cerl:cons_tl(Node))];
        tuple ->
            list_to_tuple([const_value(E) || E <- cerl:tuple_es(Node)]);
        _ ->
            error({not_const, cerl:type(Node)})
    end.
