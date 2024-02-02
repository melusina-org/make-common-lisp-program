# GitHub Action to Make Common Lisp Programs

This GitHub Action makes Common Lisp programs on GitHub runners where a
Common Lisp implementation is already installed.

[![Continuous Integration](https://github.com/melusina-org/make-common-lisp-program/actions/workflows/continuous-integration.yaml/badge.svg)](https://github.com/melusina-org/make-common-lisp-program/actions/workflows/continuous-integration.yaml)

This action is complemented by other actions related to the Common
Lisp eco system:

- [setup-common-lisp](https://github.com/melusina-org/setup-common-lisp)
- [setup-quicklisp](https://github.com/melusina-org/setup-quicklisp)
- [asdf-operate](https://github.com/melusina-org/asdf-operate)
- [make-lisp-system-documentation-texinfo](https://github.com/melusina-org/make-lisp-system-documentation-texinfo)


## Usage

Create a workflow file in the`.github/workflows` directory of your
working copy.  This workflow file should use a MacOS runner, a
Ubuntu Runner or a Windows runner and use the branch `v1` of this action.


An [example workflow](#example-workflow) is available below. See the GitHub Help Documentation for
[Creating a workflow file](https://help.github.com/en/articles/configuring-a-workflow#creating-a-workflow-file)
to get started with GitHub workflows.


## Outcomes

Once the action has been executed succesfully, the required Common
Lisp program has been made under the pathname saved in the
`build-pathname` action output.

## Inputs

* `implementation` — The Common Lisp implementation to setup QuickLisp
  for. This can be one of the values `ecl sbcl` and
  in the future we would like to support all of `abcl clasp clisp ecl gcl sbcl`
  and maybe other implementations. Please open an issue to express
  interest for other implementations.

* `system`: The Common Lisp system where the program resides. This
  common Lisp system must have specific ASDF annotations, such as
  `build-pathname`. See [ASDF Best practices](https://github.com/fare/asdf/blob/master/doc/best_practices.md)

## Outputs

* `build-pathname` — The pathname to the program made by ASDF.

## Example Workflow

```yaml
name: 'Continuous Integration'
on:
  workflow_dispatch:
  push:
    branches-ignore:
      - v1
    tags-ignore:
      - v1.*

jobs:
  exercise-on-tier-1-platforms:
    strategy:
      matrix:
        implementation: ['sbcl']
        os: ['ubuntu-latest', 'macos-latest', 'windows-latest']
    runs-on: '${{ matrix.os }}'
    name: 'Exercise on Tier 1 Platform'
    steps:
      - uses: actions/checkout@v4
      - name: 'Install MacPorts'
        if: runner.os == 'macOS'
        uses: melusina-org/setup-macports@v1
      - name: 'Setup Common Lisp'
        uses: melusina-org/setup-common-lisp@v1
        with:
          implementation: '${{ matrix.implementation }}'
      - name: 'Setup Quicklisp'
        uses: melusina-org/setup-quicklisp@v1
        id: quicklisp
        with:
          implementation: '${{ matrix.implementation }}'
      - name: 'Install CL-GITHUB-ACTIONS'
        uses: actions/checkout@v4
        with:
          repository: melusina-org/cl-github-actions
          path: ${{ steps.quicklisp.outputs.quicklisp-local-projects }}/cl-github-actions
      - name: 'Run unit tests'
        uses: melusina-org/run-common-lisp-program@v1
        with:
          implementation: '${{ matrix.implementation }}'
          system: 'org.melusina.github-action.make-common-lisp-program/testsuite'
          entrypoint: 'unit-tests'
      - name: 'Checkout Common Lisp Reference Utility'
        uses: actions/checkout@v4
        with:
          repository: melusina-org/reference-utility
          path: ${{ steps.quicklisp.outputs.quicklisp-local-projects }}/reference-utility
      - name: 'Make Common Lisp Reference Utility'
        uses: melusina-org/make-common-lisp-program@v1
        id: make
        with:
          implementation: '${{ matrix.implementation }}'
          system: 'org.melusina.reference-utility/executable'
      - name: 'Run Common Lisp Reference Utility'
        shell: sh
        run: |
          ${{ steps.make.outputs.build-pathname }}
      - name: 'Upload Common Lisp Reference Utility'
        uses: actions/upload-artifact@v3
        with:
          name: Common Lisp Reference Utility ${{ matrix.implementation }} ${{ runner.os }} ${{ runner.arch }}
          path: ${{ steps.make.outputs.build-pathname }}
```

## License
The scripts and documentation in this project are released under the [MIT License](LICENSE)
