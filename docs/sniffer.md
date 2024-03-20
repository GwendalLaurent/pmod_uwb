# Installation
To use the sniffer you must follow the instruction given on the manaufacturer website.
The installation tutorial can be found [here](https://www.sewio.net/uwb-sniffer/uwb-sniffer-installation/)

The tutorial has 3 parts:
- 1 Setting up the UWB sniffer
- 2 Getting Wireshark running
- 3 Installing ZEPv3 and Wireshark settings (optional)

Note that you *MUST* install Wireshark v 1.12.x

# Sniffer configuration

Before running the sniffer, you must set the correct PHY parameters in the **Settings** section (described [here](logger:notice("18:46:05")))

Here is the list of the values for each settings

|--------------------------|----------|
|  Setting                 |   Value  |
|--------------------------|----------|
| Channel                  |     5    |
|--------------------------|----------|
|   PRF                    |    16MHz |
|--------------------------|----------|
| Data rate                | 6,8 Mbps |
|--------------------------|----------|
| Preamble code            | 4        |
|--------------------------|----------|
| PAC size                 |   8      |
|--------------------------|----------|
| Standard frame delimiter | Standard |
|--------------------------|----------|
| PHR                      | Standard |
|--------------------------|----------|
| CRC filter               | Off      |
|--------------------------|----------|
