# Network Scanner

A simple network port scanner written in Haskell that checks for open ports on a given host.

## Features

- Scans specified TCP ports
- Checks if ports are open/closed
- Currently configured to scan common web ports (80, 443, 8080)
- Written in pure Haskell using the `network` library

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

The scanner will check ports 80, 443, and 8080 on localhost (127.0.0.1).

## Project Structure

- src
    - Library source code
- app
    - Application source code
- test
    - Test files

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
