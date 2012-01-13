package data.util;

import java.util.concurrent.atomic.AtomicReference;

public class EditContext {
    private final AtomicReference<Thread> _ctx;

    public EditContext() {
        this(new AtomicReference<Thread>(Thread.currentThread()));
    }

    public EditContext(AtomicReference<Thread> ctx) {
        this._ctx = ctx;
    }

    public void persist() {
        this._ctx.set(null);
    }

    public boolean isEditable() {
        Thread owner = this._ctx.get();
        return (owner == Thread.currentThread());
    }

    public void ensureEditable() {
        Thread owner = this._ctx.get();
        if(owner == Thread.currentThread())
            return;
        if(owner != null)
            throw new IllegalAccessError("Edit attempted by non-owner thread");
        throw new IllegalAccessError("Edit attempted after persist! call");
    }
}
