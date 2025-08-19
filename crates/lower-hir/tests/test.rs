use swamp_dep_loader::RunMode;
use swamp_semantic::ExpressionKind;

#[test]
fn test_compile() {
    let (program, module_ref, _source_map) = swamp_compile::compile_string(r#"
        a = 3
        b = a + 88
    "#, &RunMode::Deployed);
    assert!(program.state.errors.is_empty(), "was errors");

    let mut lower_hir = swamp_lower_hir::LowerHir::new(&program.state.types);

    let ExpressionKind::Block(block_expressions) = &module_ref.main_expression.as_ref().unwrap().expression.kind  else {
        panic!("no block")
    };
    let statement = lower_hir.lower_block_expressions(block_expressions);
    statement.print();
}
