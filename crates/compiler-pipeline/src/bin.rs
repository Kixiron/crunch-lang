use compiler_pipeline::CompilerPipeline;

fn main() {
    let pipeline = CompilerPipeline;

    pipeline.run("../../tests/fibonacci_recursive.crunch");
}
