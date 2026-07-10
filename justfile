default:
    @just --list

build-programs-rv32:
    make -C test/programs_rv32

test name: build-programs-rv32
    MCC_TEST_HEX={{justfile_directory()}}/test/programs_rv32/{{name}}.memhex mill mcc.test.testOnly mcc.MccProgramTest


test-trace name: build-programs-rv32
    MCC_TEST_HEX={{justfile_directory()}}/test/programs_rv32/{{name}}.memhex mill mcc.test.testOnly mcc.MccProgramTest -- -DemitVcd=1


test-all: build-programs-rv32
    #!/usr/bin/env bash
    failed=0
    for hex in test/programs_rv32/*.memhex; do
        name=$(basename "$hex" .memhex)
        echo "--- $name ---"
        MCC_TEST_HEX="{{justfile_directory()}}/$hex" mill mcc.test.testOnly mcc.MccProgramTest || failed=1
    done
    [ $failed -eq 0 ]

test-all-trace: build-programs-rv32
    #!/usr/bin/env bash
    failed=0
    for hex in test/programs_rv32/*.memhex; do
        name=$(basename "$hex" .memhex)
        echo "--- $name ---"
        MCC_TEST_HEX="{{justfile_directory()}}/$hex" mill mcc.test.testOnly mcc.MccProgramTest -- -DemitVcd=1 || failed=1
    done
    [ $failed -eq 0 ]

elab:
    mill mcc.test.testOnly mcc.MccElaborationTest
