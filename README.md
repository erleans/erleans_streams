Erleans Streams
=====

Grains can subscribe to streams and implement callbacks for handling records as they are published. Each erleans node runs a configurable number of stream agents which are responsible for the actual fetching off a stream and forwarding to the subscribed grains. Stream providers implement how to read from and publish to a given stream backend. On arrival of new events on the stream the agents makes a call to each subscriber with the new event, thus blocking for as long as the slowest subscriber takes to handle the event, before fetching any new events.

A grain must explicitly unsubscribe from a stream or it will continue to receive calls even after being deactivated. This is because subscriptions are based on the grain reference, not the activation, so if a subscribed grain has deactivated it will be reactivated on the next event on the stream.
