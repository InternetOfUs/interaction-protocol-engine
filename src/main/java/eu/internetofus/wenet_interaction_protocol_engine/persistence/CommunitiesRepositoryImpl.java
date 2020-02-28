/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.persistence;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.ext.mongo.UpdateOptions;

/**
 * Implementation of the {@link CommunitiesRepository}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class CommunitiesRepositoryImpl extends Repository implements CommunitiesRepository {

	/**
	 * The name of the collection that contains the communities.
	 */
	public static final String COMMUNITIES_COLLECTION = "communities";

	/**
	 * Create a new service.
	 *
	 * @param pool to create the connections.
	 */
	public CommunitiesRepositoryImpl(MongoClient pool) {

		super(pool);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void searchCommunityObject(String id, Handler<AsyncResult<JsonObject>> searchHandler) {

		final JsonObject query = new JsonObject().put("_id", id);
		this.pool.findOne(COMMUNITIES_COLLECTION, query, null, search -> {

			if (search.failed()) {

				searchHandler.handle(Future.failedFuture(search.cause()));

			} else {

				final JsonObject community = search.result();
				if (community == null) {

					searchHandler.handle(Future.failedFuture("Does not exist a community with the identifier '" + id + "'."));

				} else {

					searchHandler.handle(Future.succeededFuture(community));
				}
			}
		});

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void storeCommunity(JsonObject community, Handler<AsyncResult<JsonObject>> storeHandler) {

		final long now = System.currentTimeMillis();
		community.put("sinceTime", now);
		this.pool.save(COMMUNITIES_COLLECTION, community, store -> {

			if (store.failed()) {

				storeHandler.handle(Future.failedFuture(store.cause()));

			} else {

				storeHandler.handle(Future.succeededFuture(community));
			}

		});

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void updateCommunity(JsonObject community, Handler<AsyncResult<JsonObject>> updateHandler) {

		final String id = community.getString("_id");
		final JsonObject query = new JsonObject().put("_id", id);
		final JsonObject updateCommunity = new JsonObject().put("$set", community);
		final UpdateOptions options = new UpdateOptions().setMulti(false);
		this.pool.updateCollectionWithOptions(COMMUNITIES_COLLECTION, query, updateCommunity, options, update -> {

			if (update.failed()) {

				updateHandler.handle(Future.failedFuture(update.cause()));

			} else if (update.result().getDocModified() != 1) {

				updateHandler.handle(Future.failedFuture("Not Found community to update"));

			} else {

				updateHandler.handle(Future.succeededFuture(community));
			}
		});

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void deleteCommunity(String id, Handler<AsyncResult<Void>> deleteHandler) {

		final JsonObject query = new JsonObject().put("_id", id);
		this.pool.removeDocument(COMMUNITIES_COLLECTION, query, remove -> {

			if (remove.failed()) {

				deleteHandler.handle(Future.failedFuture(remove.cause()));

			} else if (remove.result().getRemovedCount() != 1) {

				deleteHandler.handle(Future.failedFuture("Not Found community to delete"));

			} else {

				deleteHandler.handle(Future.succeededFuture());
			}
		});

	}

}
