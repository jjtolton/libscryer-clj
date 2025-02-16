
    export class LeafAnswer {
        constructor(bindings) {
            this.bindings = bindings;
        }
    }

    export class PrologInteger {
        constructor(integerStr) {
            this.type = "integer";
            this.integer = BigInt(integerStr);
        }
    }

    export class PrologRational {
        constructor(numeratorStr, denominatorStr) {
            this.type = "rational";
            this.numerator = BigInt(numeratorStr);
            this.denominator = BigInt(denominatorStr);
        }
    }

    export class PrologFloat {
        constructor(float) {
            this.type = "float";
            this.float = float;
        }
    }

    export class PrologAtom {
        constructor(atom) {
            this.type = "atom";
            this.atom = atom;
        }
    }

    export class PrologString {
        constructor(string) {
            this.type = "string";
            this.string = string;
        }
    }

    export class PrologList {
        constructor(list) {
            this.type = "list";
            this.list = list;
        }
    }

    export class PrologCompound {
        constructor(functor, args) {
            this.type = "compound";
            this.functor = functor;
            this.args = args;
        }
    }

    export class PrologVariable {
        constructor(variable) {
            this.type = "variable";
            this.variable = variable;
        }
    }

    export class PrologException {
        constructor(exception) {
            this.exception = exception;
        }
    }
