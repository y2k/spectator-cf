const x = require("../_build/default/bin/main.bc.js");

export default {
    fetch(request, env, ctx) {
        return x.fetch(request, env, ctx);
    },
};