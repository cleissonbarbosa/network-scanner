# Network Scanner

This project is a simple Haskell application that scans specified ports on a given host and performs OS fingerprinting based on the TTL value from the ping command. The results are collected and reported in a user-friendly format.

## Features

- **Concurrent Port Scanning**: Uses async for high-performance concurrent scanning with configurable timeouts
- **Enhanced Service Detection**: Identifies services running on 18+ common ports including web servers, databases, and remote access services
- **SNMP Network Discovery**: Automatically discovers and analyzes network devices with detailed device information and topology mapping
- **Network Topology Mapping**: Creates comprehensive network maps showing device relationships and network segments
- **Intelligent Port Ranges**: Scans common ports (SSH, HTTP, HTTPS, FTP, etc.) plus configurable custom ranges
- **Structured Logging**: Provides timestamped logs with different levels (INFO, DEBUG, ERROR) for better troubleshooting
- **Detailed Reporting**: Generates both console output and detailed text reports with scan statistics, device information, and topology maps
- **OS Fingerprinting**: Identifies target operating system based on TTL values from ping responses
- **Active Host Discovery**: Automatically discovers active hosts in network segments
- **Device Classification**: Intelligently categorizes devices (Linux servers, Windows workstations, web servers, databases, etc.)
- **Timeout Management**: Prevents hanging connections with configurable timeout handling
- **Clean Output Format**: Uses visual indicators (✓/✗) to clearly show open/closed ports and device status
- Written in pure Haskell using the `network` library

## Prerequisites

To build and run this project, you need:

- [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler)
- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Installation

### Option 1: Download Pre-built Binaries (Recommended)

Download the latest release for your platform from the [Releases page](https://github.com/cleissonbarbosa/network-scanner/releases):

- **Linux**: `network-scanner-linux-amd64.tar.gz`
- **macOS**: `network-scanner-macos-amd64.tar.gz`
- **Windows**: `network-scanner-windows-amd64.exe.zip`

Extract and run:
```sh
# Linux/macOS
tar -xzf network-scanner-linux-amd64.tar.gz
chmod +x network-scanner-linux-amd64
./network-scanner-linux-amd64

# Windows
# Extract the ZIP file and run network-scanner-windows-amd64.exe
```

### Option 2: Build from Source

Clone the repository and build the project:

```sh
git clone https://github.com/cleissonbarbosa/network-scanner.git
cd network-scanner
stack build
```

## Usage

### Using Pre-built Binary
Run the downloaded executable:
```sh
# Linux/macOS
./network-scanner-linux-amd64

# Windows
network-scanner-windows-amd64.exe
```

### Using Source Build
Run the executable:

```sh
stack exec network-scanner-exe
```

By default, the application scans the localhost (`127.0.0.1`) for common ports and performs comprehensive network analysis including:

### Port Scanning
- Web services (HTTP/HTTPS on ports 80, 443, 8080, 8443)
- Remote access (SSH on 22, RDP on 3389, VNC on 5900)  
- Email services (SMTP, POP3, IMAP)
- Database services (MySQL, PostgreSQL)
- Custom port ranges (currently 75-85)

### SNMP Network Discovery
- Automatic discovery of active hosts in the network segment
- Device classification based on open services and fingerprinting
- Detailed device information collection (system name, description, uptime, contact, location)
- Network topology mapping with inferred connections
- Support for various device types: Linux/Windows servers, web servers, database servers, network devices

### Comprehensive Reporting
The scanner generates detailed reports including:
- Port scan results with service identification
- OS fingerprinting information
- SNMP device discovery results
- Network topology map with device relationships
- Summary statistics and device categorization

Example output:
```
📡 DISPOSITIVO: 192.168.1.10 (Linux LAMP Server)
   Nome: webserver01
   Descrição: Linux LAMP Server (SSH + HTTP + HTTPS detected)
   Uptime: 15 days, 4 hours, 32 minutes
   Contato: admin@192.168.1.10
   Localização: Network Segment

🗺️  MAPA DE TOPOLOGIA DA REDE
🔗 192.168.1.10 (Linux Server) - webserver01
  192.168.1.10 <--> Web-DMZ
  192.168.1.10 <--> Admin-Network
```

## Project Structure

```
network-scanner
├── app
│   ├── Main.hs            # Entry point with SNMP integration and enhanced reporting
│   ├── Scanner.hs         # Port scanning with timeout and service detection  
│   ├── OSFingerprint.hs   # OS fingerprinting functionality
│   ├── Logger.hs          # Structured logging system
│   ├── SNMPScanner.hs     # SNMP device discovery and topology mapping
│   └── NetworkDiscovery.hs # Active host discovery and network analysis
├── src
│   └── Lib.hs            # Library module
├── network-scanner.cabal # Project configuration file
└── README.md             # Documentation for the project
```

## Dependencies

- base >= 4.7 && < 5
- network
- bytestring
- async
- time
- process
- containers
- text

## License

This project is licensed under the BSD-3-Clause License - see the [LICENSE](LICENSE) file for details.

## Contributing

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes using conventional commits (`git commit -m 'feat: add some amazing feature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

### Releases

This project uses automated semantic versioning. When changes are merged to `main`, the CI automatically:
- Analyzes commit messages for version bumps
- Updates version in `package.yaml`
- Creates git tags
- Builds binaries for Linux, macOS, and Windows
- Publishes releases with downloadable binaries

Use conventional commit messages:
- `feat:` for new features (minor bump)
- `fix:` for bug fixes (patch bump)
- `feat!:` or `fix!:` for breaking changes (major bump)

See [RELEASE.md](RELEASE.md) for detailed release instructions.
