#!groovy
// -*- mode: groovy -*-

build('woorl', 'docker-host') {

  checkoutRepo()
  loadBuildUtils('build-utils')

  def pipeErlangLib
  runStage('load pipeline') {
    env.JENKINS_LIB = "build-utils/jenkins_lib"
    env.SH_TOOLS = "build-utils/sh"
    pipeErlangLib = load("${env.JENKINS_LIB}/pipeErlangLib.groovy")
  }

  pipeErlangLib.runPipe(false)
}

