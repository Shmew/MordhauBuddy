(**
# Contributing #

## Branching ##

 - Create a feature branch in git.
 - Fork and clone locally.

## Style Guidelines ##

 - Follow [F# Coding Guidelines](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting).
 - Take into consideration [FSharpLint](https://github.com/fsprojects/FSharpLint) output.
 - [Fantomas](https://github.com/fsprojects/fantomas) is automatically run, so code formatting will be consistent.

## Testing ##

Testing is done in the tests libraries using [Expecto](https://github.com/haf/expecto) which is able to load in any
testing framework you may need, but is strongly encouraged to keep things consistent and use Expecto tests and [FsCheck](https://github.com/fscheck/FsCheck)
for any property-based testing. 

 - Add test cases where needed.
 - Tests are run as part of the build process, please ensure they pass.

## Documentation ##

Documentation is built dynamically from F# script files and markdown files using [FSharp.Formatting](https://github.com/fsprojects/FSharp.Formatting).
The Reference documentation is dynamically generated, and descriptions specified by using `///` rather than `//` for comments, see the Formatting documentation
for more information.

 - Add documentation as necessary, pages can be modified and created in the `./docsrc` directory.
 - Documentation is built during the build process.
 - Consider adding examples if adding new features.
 - [Markdown Syntax](https://daringfireball.net/projects/markdown/syntax)

## Building ##

 - Build the solution with Visual Studio, `build.cmd` or `build.sh`.
 - All binaries will be in `./bin` after being built.
 - When you're ready to commit the code use `build.cmd/sh -t "Release" --Msg="my commit message"`. This will clean up directories, stage,
   and commit all the changes with the given message. If you don't specify a commit message it will default to `Bump version to *.*.*` which pulls the
   version from the RELEASE_NOTES.md. Do note that this will enforce that your versioning is updated every release.

<img src="img/fake.png" alt="Build process"/>

## Merging ##

 - Send a pull request and have it reviewed.

*)