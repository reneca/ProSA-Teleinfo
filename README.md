# ProSA Teleinfo processor

ProSA processor to handle Enedis (French electricity network) Teleinfo data.

This processor is designed to work with the [OVPinergy card](https://www.overware.fr/ovpinergy/), but it can also work with third-party cards that connect Enedis eletric counter through serial.
The OVPinergy card leverages the serial connection of [Raspberry Pi](https://www.raspberrypi.com/documentation/computers/configuration.html#configure-uarts).

## Configuration

For configuration, you can set the `serial_path` for the serial connection.
If not set, it'll use the first available serial port.

The `legacy` flag indicates whether data is sent in the historical format or the standard format.
This mode depend on the meter type. Linky meters handle both formats, and the configuration can be displayed on the meter's screen (TIC mode).

The rest of the configuration options are for setting prices.
This is useful if the [EDF price](https://particulier.edf.fr/fr/accueil/electricite-gaz/tarif-bleu.html) change and you haven't updated the processor, or if you have custom pricing.
Prices in the following example are for illustration purposes only and may not be accurate.

```yaml
teleinfo:
  serial_path: "/dev/serial0",
  legacy: false,
  price_base: 19.40
  price_hc_hp:
  - 15.79
  - 20.65
  price_tempo:
  - 13.25
  - 16.12
  - 14.99
  - 18.71
  - 15.75
  - 70.60
```
