/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
/*
use eira::Kind;
use std::io::stderr;
use swamp_error_report::Report;
use swamp_semantic::Span;
use source_map_cache::SourceMap;


#[test]
fn standard() {
    let report = Report::build(
        Kind::Error,
        293,
        "Illegal type parameter",
        &Span {
            file_id: 0,
            offset: 0,
            length: 5,
        },
    )
    .build();

    let mut s = SourceMap::new("tests/fixtures/".as_ref());

    s.add_manual(
        0,
        "test.swamp".as_ref(),
        r"hello, world!
     asdfjisafd",
    );

    report.print(&s, stderr()).expect("print should work");
}
*/
