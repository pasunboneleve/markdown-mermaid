# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.0] - 2025-12-10

### Changed

- Now the WIDTH and HEIGHT of the resulting PNG file is set by the
  dimensions of the first screen found by Emacs. That's less
  disappointing than 800x600, the default for `mmdc`.

- The output buffer is now called `*mermaid-image*`. That makes it
  easier when we want to remove a lot of buffers we don't need
  anymore.

- The internal functions were broken apart. We didn't add regression
  tests, but this will make that task easier later.

### Not changed

- I had the intention of creating SVGs, but by the looks of a
  [mermaid-cli
  issue](https://github.com/mermaid-js/mermaid-cli/issues/112),
  keeping the text and the edges is not straightforward. So I'll keep
  using PNG for now, and maybe later replace the engine altogether.
