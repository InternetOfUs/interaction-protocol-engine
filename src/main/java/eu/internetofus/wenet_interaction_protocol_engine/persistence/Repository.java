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
import io.vertx.ext.mongo.FindOptions;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.ext.mongo.UpdateOptions;

/**
 * A component that manage the persistence of a component.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class Repository {

	/**
	 * The code that define that the connection failed.
	 */
	public static final int CONNECTION_FAILED = 1;

	/**
	 * The pool of database connections.
	 */
	protected MongoClient pool;

	/**
	 * Create a new service.
	 *
	 * @param pool to create the connections.
	 */
	public Repository(MongoClient pool) {

		this.pool = pool;

	}

	/**
	 * Search for a page.
	 *
	 * @param collectionName of the collections that contains the models.
	 * @param query          to obtain the components of the page.
	 * @param fields         for the search.
	 * @param offset         index of the first model to obtain.
	 * @param limit          number maximum of models to return.
	 * @param resultKey      to store the found models.
	 * @param searchHandler  handler to manage the result action.
	 */
	protected void searchPageObject(String collectionName, JsonObject query, JsonObject fields, int offset, int limit,
			String resultKey, Handler<AsyncResult<JsonObject>> searchHandler) {

		this.pool.count(collectionName, query, count -> {

			if (count.failed()) {

				searchHandler.handle(Future.failedFuture(count.cause()));

			} else {

				final long total = count.result().longValue();
				final JsonObject page = new JsonObject().put("offset", offset).put("total", total);
				if (total == 0 || offset >= total) {

					searchHandler.handle(Future.succeededFuture(page));

				} else {

					final FindOptions options = new FindOptions();
					options.setLimit(limit);
					options.setSkip(offset);
					options.setFields(fields);
					this.pool.findWithOptions(collectionName, query, options, find -> {

						if (find.failed()) {

							searchHandler.handle(Future.failedFuture(find.cause()));

						} else {

							page.put(resultKey, find.result());
							searchHandler.handle(Future.succeededFuture(page));
						}

					});

				}

			}
		});
	}

	/**
	 * Delete one document.
	 *
	 * @param collectionName of the collections that contains the model to delete.
	 * @param query          to to match the document to delete.
	 * @param deleteHandler  handler to manage the delete action.
	 */
	protected void deleteOneDocument(String collectionName, JsonObject query, Handler<AsyncResult<Void>> deleteHandler) {

		this.pool.removeDocument(collectionName, query, remove -> {

			if (remove.failed()) {

				deleteHandler.handle(Future.failedFuture(remove.cause()));

			} else if (remove.result().getRemovedCount() != 1) {

				deleteHandler.handle(Future.failedFuture("Not found document to delete"));

			} else {

				deleteHandler.handle(Future.succeededFuture());
			}
		});

	}

	/**
	 * Update one document.
	 *
	 * @param collectionName of the collections that contains the model to update.
	 * @param query          to to match the document to update.
	 * @param updateModel    the new values of the model.
	 * @param updateHandler  handler to manage the update action.
	 */
	protected void updateOneDocument(String collectionName, JsonObject query, JsonObject updateModel,
			Handler<AsyncResult<JsonObject>> updateHandler) {

		final JsonObject updateQuery = new JsonObject().put("$set", updateModel);
		final UpdateOptions options = new UpdateOptions().setMulti(false);
		this.pool.updateCollectionWithOptions(collectionName, query, updateQuery, options, update -> {

			if (update.failed()) {

				updateHandler.handle(Future.failedFuture(update.cause()));

			} else if (update.result().getDocModified() != 1) {

				updateHandler.handle(Future.failedFuture("Not found document to update"));

			} else {

				updateHandler.handle(Future.succeededFuture(updateModel));
			}
		});

	}

	/**
	 * Store one document.
	 *
	 * @param collectionName of the collections that contains the model to store.
	 * @param model          to store.
	 * @param storeHandler   handler to manage the store action.
	 * @param fieldsToRemove fields to remove after stored the model.
	 */
	protected void storeOneDocument(String collectionName, JsonObject model,
			Handler<AsyncResult<JsonObject>> storeHandler, String... fieldsToRemove) {

		this.pool.save(collectionName, model, store -> {

			if (store.failed()) {

				storeHandler.handle(Future.failedFuture(store.cause()));

			} else {

				if (fieldsToRemove != null) {

					for (final String field : fieldsToRemove) {

						model.remove(field);
					}
				}
				storeHandler.handle(Future.succeededFuture(model));
			}

		});
	}

	/**
	 * Find one document.
	 *
	 * @param collectionName of the collections that contains the model to find.
	 * @param query          of the document to find.
	 * @param fields         to return.
	 * @param searchHandler  handler to manage the find action.
	 */
	protected void findOneDocument(String collectionName, JsonObject query, JsonObject fields,
			Handler<AsyncResult<JsonObject>> searchHandler) {

		this.pool.findOne(collectionName, query, fields, search -> {

			if (search.failed()) {

				searchHandler.handle(Future.failedFuture(search.cause()));

			} else {

				final JsonObject community = search.result();
				if (community == null) {

					searchHandler.handle(Future.failedFuture("Does not exist a document that match '" + query + "'."));

				} else {

					searchHandler.handle(Future.succeededFuture(community));
				}
			}
		});

	}

}
