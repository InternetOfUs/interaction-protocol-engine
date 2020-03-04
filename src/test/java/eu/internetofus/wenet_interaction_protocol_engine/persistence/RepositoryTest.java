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

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.ext.mongo.MongoClientDeleteResult;
import io.vertx.ext.mongo.MongoClientUpdateResult;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

/**
 * Test the {@link Repository}.
 *
 * @see Repository
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(VertxExtension.class)
public class RepositoryTest {

	/**
	 * Check search communities fail because can not obtain the number of
	 * communities that match.
	 *
	 * @param testContext test context.
	 */
	@Test
	public void shouldSearchPageObjectFailedByMongoClientCount(VertxTestContext testContext) {

		final Repository repository = new Repository(mock(MongoClient.class));
		repository.searchPageObject(null, null, null, 0, 100, null, testContext.failing(search -> {
			testContext.completeNow();
		}));
		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<Long>>> handler = ArgumentCaptor.forClass(Handler.class);
		verify(repository.pool, times(1)).count(any(), any(), handler.capture());
		handler.getValue().handle(Future.failedFuture("Internal error"));

	}

	/**
	 * Check search communities fail because can not find.
	 *
	 * @param testContext test context.
	 */
	@Test
	public void shouldSearchPageObjectFailedByMongoClientFind(VertxTestContext testContext) {

		final Repository repository = new Repository(mock(MongoClient.class));
		repository.searchPageObject(null, null, null, 0, 100, null, testContext.failing(search -> {
			testContext.completeNow();
		}));
		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<Long>>> handler = ArgumentCaptor.forClass(Handler.class);
		verify(repository.pool, times(1)).count(any(), any(), handler.capture());
		handler.getValue().handle(Future.succeededFuture(100L));
		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<List<JsonObject>>>> findHandler = ArgumentCaptor.forClass(Handler.class);
		verify(repository.pool, times(1)).findWithOptions(any(), any(), any(), findHandler.capture());
		findHandler.getValue().handle(Future.failedFuture("Internal error"));

	}

	/**
	 * Check delete fail by mongo client.
	 *
	 * @param testContext test context.
	 */
	@Test
	public void shouldDeleteOneDocuemntFailedByMongoClient(VertxTestContext testContext) {

		final Repository repository = new Repository(mock(MongoClient.class));
		repository.deleteOneDocument(null, null, testContext.failing(delete -> {
			testContext.completeNow();
		}));
		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<MongoClientDeleteResult>>> handler = ArgumentCaptor
				.forClass(Handler.class);
		verify(repository.pool, times(1)).removeDocument(any(), any(), handler.capture());
		handler.getValue().handle(Future.failedFuture("Internal error"));

	}

	/**
	 * Check update fail by mongo client.
	 *
	 * @param testContext test context.
	 */
	@Test
	public void shouldUpdateOneDocumentFailedByMongoClient(VertxTestContext testContext) {

		final Repository repository = new Repository(mock(MongoClient.class));
		repository.updateOneDocument(null, null, null, testContext.failing(update -> {
			testContext.completeNow();
		}));
		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<MongoClientUpdateResult>>> handler = ArgumentCaptor
				.forClass(Handler.class);
		verify(repository.pool, times(1)).updateCollectionWithOptions(any(), any(), any(), any(), handler.capture());
		handler.getValue().handle(Future.failedFuture("Internal error"));

	}

	/**
	 * Check store fail by mongo client.
	 *
	 * @param testContext test context.
	 */
	@Test
	public void shouldStoreOneDocumentFailedByMongoClient(VertxTestContext testContext) {

		final Repository repository = new Repository(mock(MongoClient.class));
		repository.storeOneDocument(null, null, testContext.failing(update -> {
			testContext.completeNow();
		}));
		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<String>>> handler = ArgumentCaptor.forClass(Handler.class);
		verify(repository.pool, times(1)).save(any(), any(), handler.capture());
		handler.getValue().handle(Future.failedFuture("Internal error"));

	}

	/**
	 * Check search fail by mongo client.
	 *
	 * @param testContext test context.
	 */
	@Test
	public void shouldSearchOneDocumentFailedByMongoClient(VertxTestContext testContext) {

		final Repository repository = new Repository(mock(MongoClient.class));
		repository.findOneDocument(null, null, null, testContext.failing(update -> {
			testContext.completeNow();
		}));
		@SuppressWarnings("unchecked")
		final ArgumentCaptor<Handler<AsyncResult<JsonObject>>> handler = ArgumentCaptor.forClass(Handler.class);
		verify(repository.pool, times(1)).findOne(any(), any(), any(), handler.capture());
		handler.getValue().handle(Future.failedFuture("Internal error"));

	}

}
