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

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.PublishedNorm;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;

/**
 * Unit test to increases coverage of the {@link NormsRepository}
 *
 * @see NormsRepository
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class NormRepositoryTest {

	/**
	 * Verify that can not found a norm because that returned by repository is not
	 * right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#searchPublishedNorm(String, Handler)
	 */
	@Test
	public void shouldNotFoundPublishedNormBecauseReturnedJsonObjectIsNotRight(VertxTestContext testContext) {

		final NormsRepository repository = new NormsRepositoryImpl(null) {

			@Override
			public void searchPublishedNormObject(String id, Handler<AsyncResult<JsonObject>> searchHandler) {

				searchHandler.handle(Future.succeededFuture(new JsonObject().put("key", "value")));

			}
		};

		repository.searchPublishedNorm("any identifier", testContext.failing(fail -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not store a norm because that returned by repository is not
	 * right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#storePublishedNorm(PublishedNorm, Handler)
	 */
	@Test
	public void shouldNotStorePublishedNormBecauseReturnedJsonObjectIsNotRight(VertxTestContext testContext) {

		final NormsRepository repository = new NormsRepositoryImpl(null) {

			@Override
			public void storePublishedNorm(JsonObject norm, Handler<AsyncResult<JsonObject>> storeHandler) {

				storeHandler.handle(Future.succeededFuture(new JsonObject().put("key", "value")));
			}
		};

		repository.storePublishedNorm(new PublishedNorm(), testContext.failing(fail -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not store a norm because that returned by repository is not
	 * right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#storePublishedNorm(PublishedNorm, Handler)
	 */
	@Test
	public void shouldNotStorePublishedNormBecauseStoreFailed(VertxTestContext testContext) {

		final Throwable cause = new IllegalArgumentException("Cause that can not be stored");
		final NormsRepository repository = new NormsRepositoryImpl(null) {

			@Override
			public void storePublishedNorm(JsonObject norm, Handler<AsyncResult<JsonObject>> storeHandler) {

				storeHandler.handle(Future.failedFuture(cause));
			}

		};

		repository.storePublishedNorm(new PublishedNorm(), testContext.failing(fail -> testContext.verify(() -> {
			testContext.completeNow();
		})));

	}

	/**
	 * Verify that can not update a norm because that returned by repository is not
	 * right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#updatePublishedNorm(PublishedNorm, Handler)
	 */
	@Test
	public void shouldNotUpdatePublishedNormBecauseReturnedJsonObjectIsNotRight(VertxTestContext testContext) {

		final NormsRepository repository = new NormsRepositoryImpl(null) {

			@Override
			public void updatePublishedNorm(JsonObject norm, Handler<AsyncResult<JsonObject>> updateHandler) {

				updateHandler.handle(Future.succeededFuture(new JsonObject().put("key", "value")));
			}

		};

		repository.updatePublishedNorm(new PublishedNorm(), testContext.failing(fail -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not update a norm because that returned by repository is not
	 * right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#updatePublishedNorm(PublishedNorm, Handler)
	 */
	@Test
	public void shouldNotUpdatePublishedNormBecauseUpdateFailed(VertxTestContext testContext) {

		final Throwable cause = new IllegalArgumentException("Cause that can not be updated");
		final NormsRepository repository = new NormsRepositoryImpl(null) {

			@Override
			public void updatePublishedNorm(JsonObject norm, Handler<AsyncResult<JsonObject>> updateHandler) {

				updateHandler.handle(Future.failedFuture(cause));
			}
		};

		repository.updatePublishedNorm(new PublishedNorm(), testContext.failing(fail -> testContext.verify(() -> {
			assertThat(fail).isEqualTo(cause);
			testContext.completeNow();
		})));

	}

}
