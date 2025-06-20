use swamp_code_gen::code_bld::CodeBuilderOptions;
use swamp_code_gen::top_state::TopLevelGenState;

#[test]
fn check_it_compiles() {
    let options = CodeBuilderOptions {
        should_show_debug: false,
    };
    let top_state = TopLevelGenState::new(options);
    assert_eq!(top_state.codegen_state.functions.len(), 0);
}
