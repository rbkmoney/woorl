#!groovy
// -*- mode: groovy -*-

build('woorl', 'docker-host') {

  checkoutRepo()
  loadBuildUtils()

  def pipeErlangLib
  runStage('load pipeline') {
    env.JENKINS_LIB = "build_utils/jenkins_lib"
    env.SH_TOOLS = "build_utils/sh"
    pipeErlangLib = load("${env.JENKINS_LIB}/pipeErlangLib.groovy")
  }

  pipeErlangLib.runPipe(false, false, 'test')
}

