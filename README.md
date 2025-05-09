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
  price_base: 25.16
  price_hc_hp:
  - 20.68
  - 27.0
  price_tempo:
  - 12.96
  - 16.09
  - 14.86
  - 18.94
  - 15.68
  - 75.62
```
