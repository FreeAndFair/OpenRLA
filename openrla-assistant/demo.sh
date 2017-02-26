#!/usr/bin/env bash
set -eu -o pipefail

setup_frontend() {
  pushd frontend
    npm install
  popd
}

setup_backend() {
  pushd backend
    stack setup
  popd
}

build_frontend() {
  pushd frontend
    npm run build
  popd
}

build_backend() {
  pushd backend
    make build
  popd
}

run_frontend() {
  pushd frontend
    npm start
  popd
}

run_backend() {
  pushd backend
    make run
  popd
}

setup_all() {
  setup_backend
  setup_frontend
}

build_all() {
  build_backend
  build_frontend
}

run_all() {
  run_backend & run_frontend && fg
}

main() {
  setup_all
  build_all
  run_all
}

main
