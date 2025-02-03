# Haskell Port Scanner

This project is a simple Haskell application that scans specified ports on a given host and performs OS fingerprinting based on the TTL value from the ping command. The results are collected and reported in a user-friendly format.

## Project Structure

```
haskell-scan-app
├── src
│   ├── Main.hs            # Entry point of the application
│   ├── Scanner.hs         # Functions related to port scanning
│   └── OSFingerprint.hs    # Functions for OS fingerprinting
├── haskell-scan-app.cabal # Project configuration file
└── README.md              # Documentation for the project
```

## Installation

To build and run this project, you need to have [GHC](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/) installed on your machine.

1. Clone the repository:
   ```
   git clone <repository-url>
   cd haskell-scan-app
   ```

2. Build the project:
   ```
   cabal build
   ```

## Usage

To run the application, use the following command:

```
cabal run
```

By default, the application scans the localhost (`127.0.0.1`) for the specified ports and performs OS fingerprinting.

## Features

- Scans multiple ports concurrently.
- Detects common services associated with open ports.
- Performs OS fingerprinting based on TTL values from ping responses.
- Generates a report of the scan results and OS fingerprinting.

## License

This project is licensed under the MIT License. See the LICENSE file for more details.