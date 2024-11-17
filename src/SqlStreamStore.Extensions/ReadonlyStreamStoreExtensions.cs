using System.Runtime.CompilerServices;
using System.Threading.Channels;
using SqlStreamStore.Streams;
using SqlStreamStore.Subscriptions;

namespace SqlStreamStore;

public static class ReadonlyStreamStoreExtensions {
	public static async IAsyncEnumerable<StreamMessage> ReadStreamForwards(this IReadonlyStreamStore store,
		string streamName, [EnumeratorCancellation] CancellationToken ct = default) {
		var from = 0;

		Loop:
		var page = await store.ReadStreamForwards(streamName, from, 32, true, ct);

		foreach (var message in page.Messages) {
			yield return message;
		}

		if (!page.IsEnd) {
			from = page.NextStreamVersion;
			goto Loop;
		}
	}

	public static async IAsyncEnumerable<StreamMessage> ReadAllForwards(this IReadonlyStreamStore store,
		[EnumeratorCancellation] CancellationToken ct = default) {
		long from = 0;

		Loop:
		var page = await store.ReadAllForwards(from, 32, true, ct);

		foreach (var message in page.Messages) {
			yield return message;
		}

		if (!page.IsEnd) {
			from = page.NextPosition;
			goto Loop;
		}
	}

	public static async IAsyncEnumerable<StreamMessage> SubscribeAllForwards(this IReadonlyStreamStore store,
		[EnumeratorCancellation] CancellationToken ct = default) {
		var channel = Channel.CreateBounded<StreamMessage>(new BoundedChannelOptions(1) {
			SingleReader = true,
			SingleWriter = true,
			FullMode = BoundedChannelFullMode.Wait
		});

		store.SubscribeToAll(null, async (_, s, ct) => {
			await channel.Writer.WriteAsync(s, ct);
		}, (_, reason, exception) => channel.Writer.TryComplete(reason switch {
			SubscriptionDroppedReason.Disposed => null,
			_ => exception ?? new Exception()
		}));

		await foreach (var message in channel.Reader.ReadAllAsync(ct)) {
			yield return message;
		}
	}
}
