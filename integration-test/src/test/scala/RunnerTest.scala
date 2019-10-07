package example.test

import minitest._
import java.io.{File, InputStream, OutputStream, StringWriter}
import java.nio.charset.StandardCharsets
import org.apache.commons.io.IOUtils

object SbtRunnerTest extends SimpleTestSuite with PowerAssertions {
  // 1.3.0, 1.3.0-M4
  private val versionRegEx = "\\d(\\.\\d+){2}(-\\w+)?"

  lazy val isWindows: Boolean = sys.props("os.name").toLowerCase(java.util.Locale.ENGLISH).contains("windows")
  lazy val sbtScript =
    if (isWindows) new File("target/universal/stage/bin/sbt.bat")
    else new File("target/universal/stage/bin/sbt")

  import sbt.internal.{InheritInput, Process, ProcessIO, ProcessLogger}

  def sbtProcess(arg: String): (Int, List[String]) = sbtProcessWithOpts(arg, "", "")
  def sbtProcessWithOpts(arg: String, javaOpts: String, sbtOpts: String): (Int, List[String]) = {
    val p = Process(sbtScript.getAbsolutePath + " " + arg, new File("citest"),
      "JAVA_OPTS" -> javaOpts,
      "SBT_OPTS" -> sbtOpts)

    // TODO: Hacks to ensure the stdout/stderr is fully read to avoid zombie windows processes
    // Is there some bug in the normal Process library using the !/!!/etc?
    val stdOutWriter: StringWriter = new StringWriter
    val stdErrWriter: StringWriter = new StringWriter

    val pio = new ProcessIO(
      writeInput = { in: (OutputStream) => in.close() },
      processOutput = { out: (InputStream) => IOUtils.copy(out, stdOutWriter, StandardCharsets.UTF_8); out.close() },
      processError = { err: (InputStream) => IOUtils.copy(err, stdErrWriter, StandardCharsets.UTF_8); err.close() },
      inheritInput = { _ => false }
    )

    val proc = p.run(pio)

    val exitValue: Int = proc.exitValue() // blocks until returned
    proc.destroy() // Ensure it is terminated

    (exitValue, stdOutWriter.toString.linesIterator.toList)
  }

  test("sbt runs") {
    assert(sbtScript.exists)
    val (ret, _) = sbtProcess("compile -v")
    assert(ret == 0)
    ()
  }

  test("sbt -no-colors") {
    val (ret, out) = sbtProcess("compile -no-colors -v")
    assert(ret == 0)
    assert(out.contains[String]("-Dsbt.log.noformat=true"))
    ()
  }

  test("sbt --no-colors") {
    val (ret, out) = sbtProcess("compile --no-colors -v")
    assert(ret == 0)
    assert(out.contains[String]("-Dsbt.log.noformat=true"))
    ()
  }

  test("sbt --color=false") {
    val (ret, out) = sbtProcess("compile --color=false -v")
    assert(ret == 0)
    assert(out.contains[String]("-Dsbt.color=false"))
    ()
  }

  test("sbt --supershell=never") {
    val (ret, out) = sbtProcess("compile --supershell=never -v")
    assert(ret == 0)
    assert(out.contains[String]("-Dsbt.supershell=never"))
    ()
  }

  test("sbt --timings") {
    val (ret, out) = sbtProcess("compile --timings -v")
    assert(ret == 0)
    assert(out.contains[String]("-Dsbt.task.timings=true"))
    ()
  }

  test("sbt --sbt-version") {
    val (ret, out) = sbtProcess("--sbt-version 1.3.0 compile -v")
    assert(ret == 0)
    assert(out.contains[String]("-Dsbt.version=1.3.0"))
    ()
  }

  test("sbt -mem 503") {
    val (ret, out) = sbtProcess("compile -mem 503 -v")
    assert(ret == 0)
    assert(out.contains[String]("-Xmx503m"))
    ()
  }
  
  test("sbt with -mem 503, -Xmx in JAVA_OPTS") {
    val (ret, out) = sbtProcessWithOpts("compile -mem 503 -v", "-Xmx1024m", "")
    assert(ret == 0)
    assert(out.contains[String]("-Xmx503m"))
    assert(!out.contains[String]("-Xms1024m"))
    ()
  }

  test("sbt with -mem 503, -Xmx in SBT_OPTS") {
    if (isWindows) cancel("Test not supported on windows")

    val (ret, out) = sbtProcessWithOpts("compile -mem 503 -v", "", "-Xmx1024m")
    assert(ret == 0)
    assert(out.contains[String]("-Xmx503m"))
    assert(!out.contains[String]("-Xmx1024m"))
    ()
  }

  test(s"sbt with -Xms2048M -Xmx2048M -Xss6M in SBT_OPTS") {
    if (isWindows) cancel("Test not supported on windows")

    val (ret, out) = sbtProcessWithOpts("compile -v", "", "-Xms2048M -Xmx2048M -Xss6M")
    assert(out.contains[String]("-Xss6M"))
    ()
  }

  test("sbt with --no-colors in SBT_OPTS") {
    if (isWindows) cancel("Test not supported on windows")

    val (ret, out) = sbtProcessWithOpts("compile -v", "", "--no-colors")
    assert(ret == 0)
    assert(out.contains[String]("-Dsbt.log.noformat=true"))
    ()
  }

  private val expectedSbtVersion =
    s"""|(?m)^sbt version in this project: $versionRegEx
        |sbt script version: $versionRegEx$$
        |""".stripMargin.trim.replace("\n", "\\n")

  test("sbt -version should print sbtVersion") {
    val (ret, out) = sbtProcessWithOpts("-version", "", "")
    assert(ret == 0)
    assert(out.mkString("\n").trim.matches(expectedSbtVersion))
    ()
  }

  test("sbt --version should print sbtVersion") {
    val (ret, out) = sbtProcessWithOpts("--version", "", "")
    assert(ret == 0)
    assert(out.mkString("\n").trim.matches(expectedSbtVersion))
    ()
  }

  test("sbt -V should print sbtVersion") {
    val (ret, out) = sbtProcessWithOpts("-V", "", "")
    assert(ret == 0)
    assert(out.mkString("\n").trim.matches(expectedSbtVersion))
    ()
  }

  test("sbt --numeric-version should print sbt script version") {
    val (ret, out) = sbtProcessWithOpts("--numeric-version", "", "")
    assert(ret == 0)
    val expectedVersion = "^"+versionRegEx+"$"
    assert(out.mkString("\n").trim.matches(expectedVersion))
    ()
  }

  test("sbt --script-version should print sbtVersion") {
    val (ret, out) = sbtProcessWithOpts("--numeric-version", "", "")
    assert(ret == 0)
    val expectedVersion = "^"+versionRegEx+"$"
    assert(out.mkString("\n").trim.matches(expectedVersion))
    ()
  }
}
