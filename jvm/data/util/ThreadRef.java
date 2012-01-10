package data.util;

import clojure.lang.IRef;
import clojure.lang.IDeref;
import java.util.concurrent.atomic.AtomicReference;

public class ThreadRef implements IDeref {
    private Object _val;
    private final AtomicReference<Thread>  _edit;

    public ThreadRef(Object val) {
        this._val = val;
        this._edit = new AtomicReference<Thread>(Thread.currentThread());
    }

    public Object deref() {
        return this._val;
    }

    public Object set(Object val) {
        ensureEditable();
        this._val = val;
        return this._val;
    }

    public ThreadRef freeze() {
        this._edit.set(null);
        return this;
    }

    void ensureEditable() {
        Thread owner = this._edit.get();
        if(owner == Thread.currentThread())
            return;
        if(owner != null)
            throw new IllegalAccessError("Reference used by non-owner thread");
        throw new IllegalAccessError("Reference used after freeze! call");
    }
}
