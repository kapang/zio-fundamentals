# ZIO Fundamentals

Writing asynchronous and concurrent applications using traditional tools, like everything under `java.util.concurrent`
is generally a very complicated task: you really need to think about what you are doing to avoid typical concurrency issues,
like deadlocks and race conditions.  And let’s be honest, thinking about all the possible scenarios that could arise is not
just hard, but also sometimes infeasible. As an alternative, libraries such as Akka (which implements the Actor Model)
have made it possible for Scala developers to build applications that are Resilient, Responsive and Scalable without the
problems mentioned before. However, Functional Effect libraries like ZIO are being used more and more to create this type
of applications, with greater type-safety, ergonomics, flexibility and testability.

Upon completion of this course, attendees will be able to build modern ZIO-based applications that are highly-performant,
resilient, efficient, easy to understand and to test; which don’t block threads nor produce deadlocks nor leak resources,
following at the same time best practices on managing errors and dependencies.

### Who Should Attend

Scala developers who would like to write modern applications that are robust, testable, and highly-performant, using the ZIO library.

### Prerequisites

Good working knowledge of Scala, including familiarity with immutable data, pattern matching, and basic recursion.

### Topics

- The basics of ZIO Functional Effects
- Error Management: Use the full power of the Scala Compiler to help you to deal with errors!
- Best practices in Error Management: Separating Recoverable Errors from Non-Recoverable Errors
- Control flow handling
- Safe Resource Management: Making Resource Leaks impossible!
- Efficient Concurrency and Parallelism with ZIO Fibers
- Sharing state in concurrent scenarios without deadlocks or race conditions
- Modularity and Dependency Management

# Usage

## From the UI

1. Download the repository as a [zip archive](https://github.com/ScalaConsultants/zio-fundamentals/archive/master.zip).
2. Unzip the archive, usually by double-clicking on the file.
3. Configure the source code files in the IDE or text editor of your choice.

## From the Command Line

1. Open up a terminal window.

2. Clone the repository.

    ```bash
    git clone https://github.com/ScalaConsultants/zio-fundamentals
    ```
5. Launch project provided `sbt`.

    ```bash
    cd zio-fundamentals; ./sbt
    ```
6. Enter continuous compilation mode.

    ```bash
    sbt:zio-fundamentals> ~ test:compile
    ```

Hint: You might get the following error when starting sbt:

> [error] 	typesafe-ivy-releases: unable to get resource for com.geirsson#sbt-scalafmt;1.6.0-RC4: res=https://repo.typesafe.com/typesafe/ivy-releases/com.geirsson/sbt-scalafmt/1.6.0-RC4/jars/sbt-scalafmt.jar: javax.net.ssl.SSLHandshakeException: sun.security.validator.ValidatorException: PKIX path building failed: sun.security.provider.certpath.SunCertPathBuilderException: unable to find valid certification path to requested targe

It's because you have an outdated Java version, missing some newer certificates. Install a newer Java version, e.g. using [Jabba](https://github.com/shyiko/jabba), a Java version manager. See [Stackoverflow](https://stackoverflow.com/a/58669704/1885392) for more details about the error.

# Legal

Copyright&copy; 2019-2021 John A. De Goes. All rights reserved.
