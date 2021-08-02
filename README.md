## Interactive Calculators for "The Case for Campaign Analysis: A Method for Studying Military Operations"

This repository contains the code used to produce three interactive calculators used in "The Case for Campaign Analysis: A Method for Studying Military Operations", published in *International Security*. All three are R Shiny code, with the main model logic in a `server.R` file, and the interactive front end in `ui.R`. Please refer to the paper and appendix for details about each of these models.

- `european_conventional_balance`: code to replicate and extend Posen's conventional military balance analysis of NATO and the Warsaw Pact in 1980s Europe.
- `merits_of_uncertainty` is a replication and extension of Wu Riqiang's 2020 article in *International Security* studying the Chinese nuclear force's susceptibility to nuclear first strikes.
- `first_strike` is a replication and extension of Lieber and Press's 2006 model of a US nuclear first strike on Russian nuclear forces, with extensions to make it match Wu's Chinese scenario more closely (e.g. options for dispersal and imperfect knowledge).

## Citation

If you use this code, please cite:

```
@article{tecott_halterman2021case,
  title={The Case for Campaign Analysis: A Method for Studying Military Operations},
  author={Tecott, Rachel and Halterman, Andrew},
  journal={International Security},
  volume={45},
  number={4},
  pages={44--83},
  year={2021},
  publisher={MIT Press},
  doi={https://doi.org/10.1162/isec_a_00408}
}
```