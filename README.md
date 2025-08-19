# Network Scanner

This project is a simple Haskell application that scans specified ports on a given host and performs OS fingerprinting based on the TTL value from the ping command. The results are collected and reported in a user-friendly format.

## Features

- **Concurrent Port Scanning**: Uses async for high-performance concurrent scanning with configurable timeouts
- **Enhanced Service Detection**: Identifies services running on 18+ common ports including web servers, databases, and remote access services
- **Intelligent Port Ranges**: Scans common ports (SSH, HTTP, HTTPS, FTP, etc.) plus configurable custom ranges
- **Structured Logging**: Provides timestamped logs with different levels (INFO, DEBUG, ERROR) for better troubleshooting
- **Detailed Reporting**: Generates both console output and detailed text reports with scan statistics
- **OS Fingerprinting**: Identifies target operating system based on TTL values from ping responses
- **Timeout Management**: Prevents hanging connections with configurable timeout handling
- **Clean Output Format**: Uses visual indicators (✓/✗) to clearly show open/closed ports
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

By default, the application scans the localhost (`127.0.0.1`) for common ports including:
- Web services (HTTP/HTTPS on ports 80, 443, 8080, 8443)
- Remote access (SSH on 22, RDP on 3389, VNC on 5900)  
- Email services (SMTP, POP3, IMAP)
- Database services (MySQL, PostgreSQL)
- Custom port ranges (currently 75-85)

The scanner provides detailed logging, generates comprehensive reports, and performs OS fingerprinting.

## Project Structure

```
network-scanner
├── app
│   ├── Main.hs            # Entry point with enhanced logging and reporting
│   ├── Scanner.hs         # Port scanning with timeout and service detection  
│   ├── OSFingerprint.hs   # OS fingerprinting functionality
│   └── Logger.hs          # Structured logging system
├── src
│   └── Lib.hs            # Library module
├── network-scanner.cabal # Project configuration file
└── README.md             # Documentation for the project
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
