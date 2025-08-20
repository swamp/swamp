use ansi_pretty_print::PrettyPrint;
use ansi_pretty_print::Printer;
use swamp_dep_loader::RunMode;
use swamp_semantic::ExpressionKind;

#[test]
fn test_mir_lowering() {
    let swamp_code = r#"
        a = 3
        b = a + 88
        if a > 3 {
           c = 3
        } else {

        }
    "#;

    eprintln!("swamp:\n{swamp_code}");
    let (program, module_ref, _source_map) =
        swamp_compile::compile_string(swamp_code, &RunMode::Deployed);
    assert!(program.state.errors.is_empty(), "was errors");

    let mut lower_hir = swamp_lower_hir::LowerHir::new(&program.state.types);

    let ExpressionKind::Block(block_expressions) =
        &module_ref.main_expression.as_ref().unwrap().expression.kind
    else {
        panic!("no block")
    };
    let block = lower_hir.lower_block_expressions(block_expressions);

    let mut buf = String::new();
    {
        let mut printer = Printer::new(&mut buf);

        block.pretty(&mut printer).expect("what happened");
        println!("done!");
    }

    eprintln!("HIR: {buf}");

    let mut lower_mir = swamp_lower_mir::LowerCtx::new();

    lower_mir.lower_fn_body(&block, None);

    let mut mir_buf = String::new();
    {
        let mut printer = Printer::new(&mut mir_buf);

        lower_mir
            .mir_func
            .pretty(&mut printer)
            .expect("what happened");
        println!("done!");
    }
    eprintln!("MIR: {mir_buf}");
}
