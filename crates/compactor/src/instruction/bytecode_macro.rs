#[macro_export]
macro_rules! bytecode {
    ($(
        $label_index:expr => {
            $( $code:tt )*
        }
    )*) => {{
        let len = [$( $label_index ),*].len();
        let mut functions: Vec<Option<Vec<$crate::Instruction>>> = alloc::vec![None; len];

        $(
            let mut bytes: Vec<$crate::Instruction> = Vec::new();
            bytecode!(@inst bytes => $($code)*);

            functions[$label_index] = Some(bytes);
        )*

        functions.into_iter().filter_map(|elem| elem).collect::<Vec<Vec<$crate::Instruction>>>()
    }};

    (@append $bytes:expr => {
        $( $code:tt )*
    }) => {
        bytecode!(@inst $bytes => $($code)*);
    };

    (@inst $bytes:expr => load null, $reg:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Load(Box::new($crate::Value::Null), $reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => load $value:expr, $reg:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Load(Box::new($value.into()), $reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => print $reg:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Print($reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => cmpr $reg:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::CompToReg($reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => opr $reg:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::OpToReg($reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => drop $reg:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Drop($reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => mov $source:expr, $target:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Move($source.into(), $target.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => copy $source:expr, $target:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Copy($source.into(), $target.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => push $reg:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Push($reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => pop $reg:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Pop($reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => add $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Add($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => sub $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Sub($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => mult $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Mult($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => div $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Div($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => jump $offset:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Jump($offset.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => jumpcmp $offset:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::JumpComp($offset.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => and $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::And($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => or $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Or($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => xor $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Xor($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => not $reg:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Not($reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => eq $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Eq($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => neq $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::NotEq($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => grt $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::GreaterThan($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => less $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::LessThan($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => grteq $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::GreaterThanEq($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => lesseq $left:expr, $right:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::LessThanEq($left.into(), $right.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => func $func:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Func($func.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => yield; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Yield);
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => gen $func:expr, $reg:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::CallGenerator($func.into(), $reg.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => pusharr $value:expr, $arr:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::PushArray { value: $value.into(), array: $arr.into() });
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => poparr $arr:expr, $target:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::PopArray { array: $arr.into(), target: $target.into() });
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => pusharr $value:expr, $arr:expr, $target:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::IndexArray { value: $value.into(), array: $arr.into(), target: $target.into() });
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => rmarr $value:expr, $arr:expr, $target:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::RemoveArray { value: $value.into(), array: $arr.into(), target: $target.into() });
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => ret; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Return);
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => collect; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Collect);
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => halt; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::Halt);
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => ldlib $name:expr, $target:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::LoadLib($name.into(), $target.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => execlib $name:expr, $target:expr, $number:expr; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::ExecLibFunc($name.into(), $target.into(), $number.into()));
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr => noop; $($rest:tt)*) => {
        $bytes.push($crate::Instruction::NoOp);
        bytecode!(@inst $bytes => $($rest)*);
    };

    (@inst $bytes:expr =>) => {};
}

#[test]
fn macro_test() {
    let functions = bytecode! {
        0 => {
            load 10i32, 0;
            print 0;
            load null, 0;
            ret;
        }
    };

    let mut stdout = std::io::stdout();
    crate::Compactor::with_stdout(Box::new(&mut stdout))
        .execute(&functions)
        .unwrap();
}
