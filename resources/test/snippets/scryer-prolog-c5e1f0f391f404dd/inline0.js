
    export function self_iterable(obj) {
        obj[Symbol.iterator] = function () {
            return this
        };
    }
