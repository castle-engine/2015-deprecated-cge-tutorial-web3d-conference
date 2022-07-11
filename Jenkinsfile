/* -*- mode: groovy -*-
  Confgure how to run our job in Jenkins.
  See https://github.com/castle-engine/castle-engine/wiki/Cloud-Builds-(Jenkins) .
*/

pipeline {
  triggers {
    pollSCM('H/4 * * * *')
    upstream(upstreamProjects: 'castle_game_engine_update_docker_image', threshold: hudson.model.Result.SUCCESS)
  }
  agent {
    docker {
      image 'kambi/castle-engine-cloud-builds-tools:cge-unstable'
    }
  }
  stages {
    stage('Build CGE Lazarus packages (by lazbuild)') {
      steps {
        sh 'echo "Castle Game Engine in ${CASTLE_ENGINE_PATH}"'
        sh 'lazbuild --add-package-link "${CASTLE_ENGINE_PATH}"src/vampyre_imaginglib/src/Packages/VampyreImagingPackage.lpk'
        sh 'lazbuild --add-package-link "${CASTLE_ENGINE_PATH}"src/vampyre_imaginglib/src/Packages/VampyreImagingPackageExt.lpk'
        sh 'lazbuild --add-package-link "${CASTLE_ENGINE_PATH}"packages/castle_base.lpk'
        sh 'lazbuild --add-package-link "${CASTLE_ENGINE_PATH}"packages/castle_window.lpk'
        sh 'lazbuild --add-package-link "${CASTLE_ENGINE_PATH}"packages/castle_components.lpk'
      }
    }
    stage('Build With Lazarus') {
      steps {
        sh 'lazbuild first_3d_application/project1.lpi'
        sh 'lazbuild 2d_game/project1.lpi'
        sh 'lazbuild fps_game/project1.lpi'
      }
    }
    stage('Build With CGE Build Tool') {
      steps {
        sh 'cd 2d_game_android_and_desktop/ && castle-engine package --os=win64 --cpu=x86_64 --verbose'
        sh 'cd 2d_game_android_and_desktop/ && castle-engine package --os=win32 --cpu=i386 --verbose'
        sh 'cd 2d_game_android_and_desktop/ && castle-engine package --os=linux --cpu=x86_64 --verbose'
        // TODO: Android debug build does not work now in Docker+Jenkins
        // sh 'cd 2d_game_android_and_desktop/ && castle-engine package --target=android --verbose'
      }
    }
  }
  post {
    regression {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build started failing: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    failure {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build failed: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    fixed {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build is again successfull: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
  }
}
