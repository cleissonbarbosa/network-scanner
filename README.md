# Network Scanner

This project is a simple Haskell application that scans specified ports on a given host and performs OS fingerprinting based on the TTL value from the ping command. The results are collected and reported in a user-friendly format.

## Features

- Scans specified TCP ports
- Checks if ports are open/closed
- Currently configured to scan common web ports and a range of ports from 75 to 85
- Written in pure Haskell using the `network` library
- OS fingerprinting based on the TTL value from the ping command

## Prerequisites

To build and run this project, you need:

- [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler)
- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Installation

Clone the repository and build the project:

```sh
git clone https://github.com/cleissonbarbosa/network-scanner.git
cd network-scanner
stack build
```

## Usage

Run the executable:

```sh
stack exec network-scanner-exe
```

By default, the application scans the localhost (`127.0.0.1`) for the specified ports and performs OS fingerprinting.

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

## Dependencies

- base >= 4.7 && < 5
- network
- bytestring

## License

This project is licensed under the BSD-3-Clause License - see the [LICENSE](LICENSE) file for details.

## Contributing

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request
