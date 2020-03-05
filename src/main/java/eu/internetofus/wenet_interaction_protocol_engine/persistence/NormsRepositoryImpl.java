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

import eu.internetofus.wenet_interaction_protocol_engine.TimeManager;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;

/**
 * Implementation of the {@link NormsRepository}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class NormsRepositoryImpl extends Repository implements NormsRepository {

	/**
	 * The name of the collection that contains the published norms.
	 */
	public static final String PUBLISHED_NORMS_COLLECTION = "publishedNorms";

	/**
	 * Create a new service.
	 *
	 * @param pool to create the connections.
	 */
	public NormsRepositoryImpl(MongoClient pool) {

		super(pool);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void searchPublishedNormObject(String id, Handler<AsyncResult<JsonObject>> searchHandler) {

		final JsonObject query = new JsonObject().put("_id", id);
		this.findOneDocument(PUBLISHED_NORMS_COLLECTION, query, null, searchHandler);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void storePublishedNorm(JsonObject norm, Handler<AsyncResult<JsonObject>> storeHandler) {

		final long now = TimeManager.now();
		norm.put("publishTime", now);
		this.storeOneDocument(PUBLISHED_NORMS_COLLECTION, norm, storeHandler);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void updatePublishedNorm(JsonObject norm, Handler<AsyncResult<JsonObject>> updateHandler) {

		final String id = norm.getString("_id");
		final JsonObject query = new JsonObject().put("_id", id);
		this.updateOneDocument(PUBLISHED_NORMS_COLLECTION, query, norm, updateHandler);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void deletePublishedNorm(String id, Handler<AsyncResult<Void>> deleteHandler) {

		final JsonObject query = new JsonObject().put("_id", id);
		this.deleteOneDocument(PUBLISHED_NORMS_COLLECTION, query, deleteHandler);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void searchPublishedNormsPageObject(JsonObject query, int offset, int limit,
			Handler<AsyncResult<JsonObject>> searchHandler) {

		this.searchPageObject(PUBLISHED_NORMS_COLLECTION, query, null, offset, limit, "norms", searchHandler);

	}

}
