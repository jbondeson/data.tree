package data.util;

import clojure.lang.IDeref;

public class ThreadBoundRef implements IDeref {
    private Object _val;
    private final EditContext  _edit;

    public ThreadBoundRef(Object val) {
        this(val, new EditContext());
    }

    public ThreadBoundRef(Object val, EditContext edit) {
        this._val = val;
        this._edit = edit;
    }

    public Object deref() {
        return this._val;
    }

    public Object set(Object val) {
        this.ensureEditable();
        this._val = val;
        return this._val;
    }

    public void ensureEditable() {
        this._edit.ensureEditable();
    }

    public boolean isEditable() {
        return this._edit.isEditable();
    }
}
