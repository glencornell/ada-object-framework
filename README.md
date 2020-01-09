# Ada Object Framework

This project is intended to provide an object framework, much like the
QtCore library for C++.  This is not a Qt binding for Ada.  Rather, it
is a pure Ada implementation that incorporates the following concepts:

* Signals and Slots
* Properties
* Object heirarchies

## Description

TODO: describe the goals of the project, use-cases, and software architecture.

## Examples

Please explore the examples for how to use this library. Summary of examples:

|Directory|Description|
|---|---|
|[signals](examples/signals)|Simple usage of signals and slots|
|[chained_signals](examples/chained_signals)|Signals that invoke other signals for delegation|
|[properties](examples/properties)|Simple use of properties|
|[object_hierarchy](examples/object_hierarchy)|Demonstration of built-in object hierarchies|

## Contributing

Comments and patches are welcome.

1. Fork it: `git clone https://github.com/glencornell/ada-object-framework`
2. Create your feature branch: `git checkout -b my-new-feature`
3. Commit your changes: `git commit -am 'Add some feature'`
4. Push to the branch: `git push origin my-new-feature`
5. Submit a pull request

## Authors

* **Glen Cornell** - Initial contribution

## License

Please refer to the [MIT license](LICENSE)

## TODO

* Add tasking support
