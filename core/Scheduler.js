"use strict";
class SchedulerEvent {
    constructor(id, ticks, callback) {
        this.nextEvent = null;
        this.prevEvent = null;
        this.id = id;
        this.ticks = ticks;
        this.callback = callback;
    }
}
var SchedulerId;
(function (SchedulerId) {
    SchedulerId[SchedulerId["None"] = 255] = "None";
    SchedulerId[SchedulerId["RootNode"] = 254] = "RootNode";
    SchedulerId[SchedulerId["Ppu"] = 0] = "Ppu";
})(SchedulerId || (SchedulerId = {}));
class Scheduler {
    constructor() {
        this.eventsQueued = 0;
        this.nextEventTicks = 0;
        this.freeEventStackIndex = 0;
        this.freeEventStack = new Array(64);
        this.returnEvent = Scheduler.createEmptyEvent();
        for (let i = 0; i < 64; i++) {
            this.freeEventStack[i] = new SchedulerEvent(SchedulerId.None, 0, () => { });
        }
        let evt = this.popStack();
        evt.id = SchedulerId.RootNode;
        evt.ticks = 0;
        this.rootEvent = evt;
        Object.seal(this);
    }
    popStack() {
        return this.freeEventStack[this.freeEventStackIndex++];
    }
    pushStack(schedulerEvent) {
        this.freeEventStack[--this.freeEventStackIndex] = schedulerEvent;
    }
    static createEmptyEvent() {
        return new SchedulerEvent(SchedulerId.None, 0, () => { });
    }
    addEvent(id, ticks, callback) {
        let newEvt = this.popStack();
        newEvt.id = id;
        newEvt.ticks = ticks;
        newEvt.callback = callback;
        let prevEvt = this.rootEvent;
        // Traverse linked list and splice at correct location
        while (prevEvt.nextEvent != null) {
            if (ticks >= prevEvt.ticks && ticks <= prevEvt.nextEvent.ticks) {
                break;
            }
            prevEvt = prevEvt.nextEvent;
        }
        let nextEvt = prevEvt.nextEvent;
        if (nextEvt != null) {
            nextEvt.prevEvent = newEvt;
        }
        prevEvt.nextEvent = newEvt;
        newEvt.nextEvent = nextEvt;
        newEvt.prevEvent = prevEvt;
        this.eventsQueued++;
        this.updateNextEvent();
    }
    cancelEventsById(id) {
        let events = [];
        let evt = this.rootEvent.nextEvent;
        while (evt != null) {
            if (evt.id == id) {
                events.push(evt);
            }
            evt = evt.nextEvent;
        }
        for (let evt of events) {
            this.removeEvent(evt);
        }
    }
    updateNextEvent() {
        if (this.eventsQueued > 0) {
            this.nextEventTicks = this.rootEvent.nextEvent.ticks;
        }
    }
    getFirstEvent() {
        return this.rootEvent.nextEvent;
    }
    popFirstEvent() {
        var evt = this.rootEvent.nextEvent;
        this.removeEvent(evt);
        return evt;
    }
    removeEvent(schedulerEvent) {
        if (schedulerEvent == this.rootEvent) {
            throw "Cannot remove root event!";
        }
        var prev = schedulerEvent.prevEvent;
        var next = schedulerEvent.nextEvent;
        if (schedulerEvent.nextEvent != null) {
            next.prevEvent = prev;
        }
        prev.nextEvent = next;
        schedulerEvent.nextEvent = null;
        schedulerEvent.prevEvent = null;
        this.eventsQueued--;
        this.updateNextEvent();
        this.pushStack(schedulerEvent);
    }
}
