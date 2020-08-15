#[cfg(test)]
#[test]
fn ddlog_test() -> Result<(), String> {
    use differential_datalog::{
        program::Update,
        record::{CollectionKind, Record},
        DDlog,
    };
    use std::borrow::Cow;
    use typecheck_ddlog::{api::HDDlog, relval_from_record, Relations};

    let (mut program, _init_state) = HDDlog::run(12, false, |_, _: &Record, _| {})?;

    let expr_table = HDDlog::get_table_id("Expr").unwrap();
    let expr1 = Record::PosStruct(
        Cow::from("Expr"),
        vec![
            Record::Int(0.into()),
            Record::PosStruct(
                Cow::from("hir::Lit"),
                vec![Record::PosStruct(
                    Cow::from("hir::Integer"),
                    vec![Record::Int(10.into())],
                )],
            ),
            Record::PosStruct(Cow::from("hir::Unknown"), vec![]),
        ],
    );
    let expr2 = Record::PosStruct(
        Cow::from("Expr"),
        vec![
            Record::Int(2.into()),
            Record::PosStruct(
                Cow::from("hir::Assign"),
                vec![Record::Int(0.into()), Record::Int(0.into())],
            ),
            Record::PosStruct(Cow::from("hir::Unknown"), vec![]),
        ],
    );
    let expr3 = Record::PosStruct(
        Cow::from("Expr"),
        vec![
            Record::Int(1.into()),
            Record::PosStruct(Cow::from("hir::Variable"), vec![Record::Int(0.into())]),
            Record::PosStruct(Cow::from("hir::Unknown"), vec![]),
        ],
    );

    let item_table = HDDlog::get_table_id("InputItems").unwrap();
    let item = Record::PosStruct(
        Cow::from("InputItems"),
        vec![Record::PosStruct(
            Cow::from("hir::ItemFunc"),
            vec![Record::PosStruct(
                Cow::from("hir::Function"),
                vec![
                    Record::Array(CollectionKind::Vector, vec![Record::Int(0.into())]),
                    Record::PosStruct(Cow::from("hir::FileLocal"), vec![]),
                    Record::Array(
                        CollectionKind::Vector,
                        vec![Record::PosStruct(
                            Cow::from("hir::FuncArg"),
                            vec![
                                Record::Int(0.into()),
                                Record::PosStruct(Cow::from("hir::Unknown"), vec![]),
                            ],
                        )],
                    ),
                    Record::PosStruct(
                        Cow::from("hir::StmtSeq"),
                        vec![
                            Record::PosStruct(
                                Cow::from("hir::StmtExpr"),
                                vec![Record::Int(0.into())],
                            ),
                            Record::PosStruct(
                                Cow::from("hir::StmtExpr"),
                                vec![Record::Int(2.into())],
                            ),
                        ],
                    ),
                    Record::PosStruct(Cow::from("hir::Unknown"), vec![]),
                ],
            )],
        )],
    );

    let mut updates = Vec::new();
    updates.push(Update::Insert {
        relid: Relations::Expr as usize,
        v: relval_from_record(expr_table, &expr1).unwrap(),
    });
    updates.push(Update::Insert {
        relid: Relations::Expr as usize,
        v: relval_from_record(expr_table, &expr2).unwrap(),
    });
    updates.push(Update::Insert {
        relid: Relations::Expr as usize,
        v: relval_from_record(expr_table, &expr3).unwrap(),
    });
    updates.push(Update::Insert {
        relid: Relations::InputItems as usize,
        v: relval_from_record(item_table, &item).unwrap(),
    });

    // an entire transaction. changes execute on commit
    program.transaction_start()?;
    program.apply_valupdates(updates.clone().into_iter())?;
    program.transaction_commit_dump_changes()?;

    program.stop()
}
