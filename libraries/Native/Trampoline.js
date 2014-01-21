Elm.Native.Trampoline = {};
Elm.Native.Trampoline.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Trampoline = elm.Native.Trampoline || {};
    if (elm.Native.Trampoline.values) return elm.Native.Trampoline.values;

    var _E = Elm.Native.Error.make(elm),

    // trampoline : Trampoline a -> a
    trampoline = function(t) {
        var tramp = t;
        while(tramp.ctor=="Continue"){
            tramp = tramp._0({ctor: "_Tuple0"});
        }//We've reached "Done"!
        return tramp._0;
    }

    return elm.Native.Trampoline.values = { trampoline: trampoline };
};
