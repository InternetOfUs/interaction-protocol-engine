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

import java.util.List;

import eu.internetofus.common.api.models.Model;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.PublishedNorm;
import io.vertx.codegen.annotations.GenIgnore;
import io.vertx.codegen.annotations.ProxyGen;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.serviceproxy.ServiceBinder;

/**
 * The service to manage the {@link PublishedNorm} on the database.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ProxyGen
public interface NormsRepository {

	/**
	 * The address of this service.
	 */
	String ADDRESS = "wenet_interaction_protocol_engine.persistence.norms";

	/**
	 * Register this service.
	 *
	 * @param vertx that contains the event bus to use.
	 * @param pool  to create the database connections.
	 */
	static void register(Vertx vertx, MongoClient pool) {

		new ServiceBinder(vertx).setAddress(NormsRepository.ADDRESS).register(NormsRepository.class,
				new NormsRepositoryImpl(pool));

	}

	/**
	 * Create a proxy of the {@link NormsRepository}.
	 *
	 * @param vertx where the service has to be used.
	 *
	 * @return the norm.
	 */
	static NormsRepository createProxy(Vertx vertx) {

		return new NormsRepositoryVertxEBProxy(vertx, NormsRepository.ADDRESS);
	}

	/**
	 * Search for the published norm with the specified identifier.
	 *
	 * @param id            identifier of the norm to search.
	 * @param searchHandler handler to manage the search.
	 */
	@GenIgnore
	default void searchPublishedNorm(String id, Handler<AsyncResult<PublishedNorm>> searchHandler) {

		this.searchPublishedNormObject(id, search -> {

			if (search.failed()) {

				searchHandler.handle(Future.failedFuture(search.cause()));

			} else {

				final JsonObject value = search.result();
				final PublishedNorm norm = Model.fromJsonObject(value, PublishedNorm.class);
				if (norm == null) {

					searchHandler.handle(Future.failedFuture("The stored published norm is not valid."));

				} else {

					searchHandler.handle(Future.succeededFuture(norm));
				}
			}
		});
	}

	/**
	 * Search for the published norm with the specified identifier.
	 *
	 * @param id            identifier of the norm to search.
	 * @param searchHandler handler to manage the search.
	 */
	void searchPublishedNormObject(String id, Handler<AsyncResult<JsonObject>> searchHandler);

	/**
	 * Store a published norm.
	 *
	 * @param norm         to store.
	 * @param storeHandler handler to manage the store.
	 */
	@GenIgnore
	default void storePublishedNorm(PublishedNorm norm, Handler<AsyncResult<PublishedNorm>> storeHandler) {

		final JsonObject object = norm.toJsonObject();
		if (object == null) {

			storeHandler.handle(Future.failedFuture("The published norm can not converted to JSON."));

		} else {

			this.storePublishedNorm(object, stored -> {
				if (stored.failed()) {

					storeHandler.handle(Future.failedFuture(stored.cause()));

				} else {

					final JsonObject value = stored.result();
					final PublishedNorm storedNorm = Model.fromJsonObject(value, PublishedNorm.class);
					if (storedNorm == null) {

						storeHandler.handle(Future.failedFuture("The stored published norm is not valid."));

					} else {

						storeHandler.handle(Future.succeededFuture(storedNorm));
					}

				}
			});
		}
	}

	/**
	 * Store a published norm.
	 *
	 * @param norm         to store.
	 * @param storeHandler handler to manage the search.
	 */
	void storePublishedNorm(JsonObject norm, Handler<AsyncResult<JsonObject>> storeHandler);

	/**
	 * Update a published norm.
	 *
	 * @param norm          to update.
	 * @param updateHandler handler to manage the update.
	 */
	@GenIgnore
	default void updatePublishedNorm(PublishedNorm norm, Handler<AsyncResult<Void>> updateHandler) {

		final JsonObject object = norm.toJsonObject();
		if (object == null) {

			updateHandler.handle(Future.failedFuture("The published norm can not converted to JSON."));

		} else {

			this.updatePublishedNorm(object, updateHandler);
		}
	}

	/**
	 * Update a published norm.
	 *
	 * @param norm          to update.
	 * @param updateHandler handler to manage the update result.
	 */
	void updatePublishedNorm(JsonObject norm, Handler<AsyncResult<Void>> updateHandler);

	/**
	 * Delete a published norm.
	 *
	 * @param id            identifier of the norm to delete.
	 * @param deleteHandler handler to manage the delete result.
	 */
	void deletePublishedNorm(String id, Handler<AsyncResult<Void>> deleteHandler);

	/**
	 * Search for the published norms that satisfy the query.
	 *
	 * @param name          of the published norms to return or a Perl compatible
	 *                      regular expressions (PCRE) that has to match the name of
	 *                      the published norms to return.
	 * @param description   of the published norms to return or a Perl compatible
	 *                      regular expressions (PCRE) that has to match the
	 *                      description of the published norms to return.
	 * @param keywords      of the published norms to return or a Perl compatible
	 *                      regular expressions (PCRE) that has to match the keyword
	 *                      of the norms to return.
	 * @param publisherId   of the published norms to return or a Perl compatible
	 *                      regular expressions (PCRE) that has to match the
	 *                      publisher identifier.
	 * @param publishFrom   time stamp inclusive that mark the older limit in witch
	 *                      the norm has been published. It is the difference,
	 *                      measured in seconds, between the time when the norm was
	 *                      published and midnight, January 1, 1970 UTC.
	 * @param publishTo     time stamp inclusive that mark the newest limit in witch
	 *                      the norm has been published. It is the difference,
	 *                      measured in seconds, between the time when the norm was
	 *                      published and midnight, January 1, 1970 UTC.
	 * @param offset        index of the first norm to return.
	 * @param limit         number maximum of norms to return.
	 * @param searchHandler handler to manage the search.
	 */
	@GenIgnore
	default void searchPublishedNormsPageObject(String name, String description, List<String> keywords,
			String publisherId, Long publishFrom, Long publishTo, int offset, int limit,
			Handler<AsyncResult<JsonObject>> searchHandler) {

		final JsonObject query = new QueryBuilder().withRegex("name", name).withRegex("description", description)
				.withRegex("keywords", keywords).withRegex("publisherId", publisherId)
				.withRange("publishTime", publishFrom, publishTo).build();
		this.searchPublishedNormsPageObject(query, offset, limit, searchHandler);

	}

	/**
	 * Search for the published norms that satisfy the query.
	 *
	 * @param query         that has to match the published norms to search.
	 * @param offset        index of the first norm to return.
	 * @param limit         number maximum of norms to return.
	 * @param searchHandler handler to manage the search.
	 */
	void searchPublishedNormsPageObject(JsonObject query, int offset, int limit,
			Handler<AsyncResult<JsonObject>> searchHandler);

}
