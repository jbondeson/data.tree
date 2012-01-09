package data.util;

import clojure.lang.IRef;
import clojure.lang.IDeref;

public class ThreadRef implements IDeref {
    private Object _val;

    public ThreadRef(Object val) {
        this._val = val;
    }

    public Object deref() {
        return this._val;
    }
}
