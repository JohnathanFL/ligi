const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    const exe = b.addExecutable("ligi", "src/main.zig");
    exe.setBuildMode(mode);
    exe.install();

    var main_tests = b.addTest("src/main.zig");
    main_tests.setBuildMode(mode);

    var run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    var run_step = b.step("run", "Run the compiler");
    run_step.dependOn(&run_cmd.step);
    

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
}
