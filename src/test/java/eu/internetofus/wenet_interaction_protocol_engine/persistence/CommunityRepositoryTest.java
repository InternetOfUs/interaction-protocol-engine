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
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.Community;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.CommunityMember;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.CommunityNorm;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;

/**
 * Unit test to increases coverage of the {@link CommunitiesRepository}
 *
 * @see CommunitiesRepository
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class CommunityRepositoryTest {

	/**
	 * Verify that can not found a community because that returned by repository is
	 * not right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunity(String, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotFoundCommunityBecauseReturnedJsonObjectIsNotRight(VertxTestContext testContext) {

		final CommunitiesRepository repository = new CommunitiesRepositoryImpl(null) {

			@Override
			public void searchCommunityObject(String id, Handler<AsyncResult<JsonObject>> searchHandler) {

				searchHandler.handle(Future.succeededFuture(new JsonObject().put("key", "value")));

			}
		};

		repository.searchCommunity("any identifier", testContext.failing(fail -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not store a community because that returned by repository is
	 * not right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunity(Community, Handler)
	 */
	@Test
	public void shouldNotStoreCommunityBecauseReturnedJsonObjectIsNotRight(VertxTestContext testContext) {

		final CommunitiesRepository repository = new CommunitiesRepositoryImpl(null) {

			@Override
			public void storeCommunity(JsonObject community, Handler<AsyncResult<JsonObject>> storeHandler) {

				storeHandler.handle(Future.succeededFuture(new JsonObject().put("key", "value")));
			}
		};

		repository.storeCommunity(new Community(), testContext.failing(fail -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not store a community because that returned by repository is
	 * not right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunity(Community, Handler)
	 */
	@Test
	public void shouldNotStoreCommunityBecauseStoreFailed(VertxTestContext testContext) {

		final Throwable cause = new IllegalArgumentException("Cause that can not be stored");
		final CommunitiesRepository repository = new CommunitiesRepositoryImpl(null) {

			@Override
			public void storeCommunity(JsonObject community, Handler<AsyncResult<JsonObject>> storeHandler) {

				storeHandler.handle(Future.failedFuture(cause));
			}

		};

		repository.storeCommunity(new Community(), testContext.failing(fail -> testContext.verify(() -> {
			testContext.completeNow();
		})));

	}

	/**
	 * Verify that can not update a community because that returned by repository is
	 * not right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunity(Community, Handler)
	 */
	@Test
	public void shouldNotUpdateCommunityBecauseReturnedJsonObjectIsNotRight(VertxTestContext testContext) {

		final CommunitiesRepository repository = new CommunitiesRepositoryImpl(null) {

			@Override
			public void updateCommunity(JsonObject community, Handler<AsyncResult<JsonObject>> updateHandler) {

				updateHandler.handle(Future.succeededFuture(new JsonObject().put("key", "value")));
			}

		};

		repository.updateCommunity(new Community(), testContext.failing(fail -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not update a community because that returned by repository is
	 * not right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunity(Community, Handler)
	 */
	@Test
	public void shouldNotUpdateCommunityBecauseUpdateFailed(VertxTestContext testContext) {

		final Throwable cause = new IllegalArgumentException("Cause that can not be updated");
		final CommunitiesRepository repository = new CommunitiesRepositoryImpl(null) {

			@Override
			public void updateCommunity(JsonObject community, Handler<AsyncResult<JsonObject>> updateHandler) {

				updateHandler.handle(Future.failedFuture(cause));
			}
		};

		repository.updateCommunity(new Community(), testContext.failing(fail -> testContext.verify(() -> {
			assertThat(fail).isEqualTo(cause);
			testContext.completeNow();
		})));

	}

	/**
	 * Verify that can not found a community member because that returned by
	 * repository is not right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMember(String,String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotFoundCommunityMemberBecauseReturnedJsonObjectIsNotRight(VertxTestContext testContext) {

		final CommunitiesRepository repository = new CommunitiesRepositoryImpl(null) {

			@Override
			public void searchCommunityMemberObject(String communityId, String userId,
					Handler<AsyncResult<JsonObject>> searchHandler) {

				searchHandler.handle(Future.succeededFuture(new JsonObject().put("key", "value")));

			}
		};

		repository.searchCommunityMember("communityId", "any identifier", testContext.failing(fail -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not store a community member because that returned by
	 * repository is not right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMember(String, String, Handler)
	 */
	@Test
	public void shouldNotStoreCommunityMemberBecauseReturnedJsonObjectIsNotRight(VertxTestContext testContext) {

		final CommunitiesRepository repository = new CommunitiesRepositoryImpl(null) {

			@Override
			public void storeCommunityMemberObject(String communityId, JsonObject community,
					Handler<AsyncResult<JsonObject>> storeHandler) {

				storeHandler.handle(Future.succeededFuture(new JsonObject().put("key", "value")));
			}
		};

		repository.storeCommunityMember("communityId", new CommunityMember(), testContext.failing(fail -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not store a community member because that returned by
	 * repository is not right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunityMember(String, CommunityMember,
	 *      Handler)
	 */
	@Test
	public void shouldNotStoreCommunityMemberBecauseStoreFailed(VertxTestContext testContext) {

		final Throwable cause = new IllegalArgumentException("Cause that can not be stored");
		final CommunitiesRepository repository = new CommunitiesRepositoryImpl(null) {

			@Override
			public void storeCommunityMemberObject(String communityId, JsonObject community,
					Handler<AsyncResult<JsonObject>> storeHandler) {

				storeHandler.handle(Future.failedFuture(cause));
			}

		};

		repository.storeCommunityMember("communityId", new CommunityMember(),
				testContext.failing(fail -> testContext.verify(() -> {
					assertThat(fail).isEqualTo(cause);
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can not update a community member because that returned by
	 * repository is not right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunityMember(String, CommunityMember,
	 *      Handler)
	 */
	@Test
	public void shouldNotUpdateCommunityMemberBecauseReturnedJsonObjectIsNotRight(VertxTestContext testContext) {

		final CommunitiesRepository repository = new CommunitiesRepositoryImpl(null) {

			@Override
			public void updateCommunityMemberObject(String communityId, JsonObject community,
					Handler<AsyncResult<JsonObject>> updateHandler) {

				updateHandler.handle(Future.succeededFuture(new JsonObject().put("key", "value")));
			}

		};

		repository.updateCommunityMember("communityId", new CommunityMember(), testContext.failing(fail -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not update a community member because that returned by
	 * repository is not right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunityMember(String,CommunityMember,Handler)
	 */
	@Test
	public void shouldNotUpdateCommunityMemberBecauseUpdateFailed(VertxTestContext testContext) {

		final Throwable cause = new IllegalArgumentException("Cause that can not be updated");
		final CommunitiesRepository repository = new CommunitiesRepositoryImpl(null) {

			/**
			 * {@inheritDoc}
			 */
			@Override
			public void updateCommunityMemberObject(String communityId, JsonObject member,
					Handler<AsyncResult<JsonObject>> updateHandler) {

				updateHandler.handle(Future.failedFuture(cause));
			}
		};

		repository.updateCommunityMember("communityId", new CommunityMember(),
				testContext.failing(fail -> testContext.verify(() -> {
					assertThat(fail).isEqualTo(cause);
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can not found a community norm because that returned by
	 * repository is not right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityNorm(String,String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotFoundCommunityNormBecanormeturnedJsonObjectIsNotRight(VertxTestContext testContext) {

		final CommunitiesRepository repository = new CommunitiesRepositoryImpl(null) {

			@Override
			public void searchCommunityNormObject(String communityId, String normId,
					Handler<AsyncResult<JsonObject>> searchHandler) {

				searchHandler.handle(Future.succeededFuture(new JsonObject().put("key", "value")));

			}
		};

		repository.searchCommunityNorm("communityId", "any identifier", testContext.failing(fail -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not store a community norm because that returned by
	 * repository is not right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityNorm(String, String, Handler)
	 */
	@Test
	public void shouldNotStoreCommunityNormBecanormeturnedJsonObjectIsNotRight(VertxTestContext testContext) {

		final CommunitiesRepository repository = new CommunitiesRepositoryImpl(null) {

			@Override
			public void storeCommunityNormObject(String communityId, JsonObject community,
					Handler<AsyncResult<JsonObject>> storeHandler) {

				storeHandler.handle(Future.succeededFuture(new JsonObject().put("key", "value")));
			}
		};

		repository.storeCommunityNorm("communityId", new CommunityNorm(), testContext.failing(fail -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not store a community norm because that returned by
	 * repository is not right.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunityNorm(String, CommunityNorm, Handler)
	 */
	@Test
	public void shouldNotStoreCommunityNormBecauseStoreFailed(VertxTestContext testContext) {

		final Throwable cause = new IllegalArgumentException("Cause that can not be stored");
		final CommunitiesRepository repository = new CommunitiesRepositoryImpl(null) {

			@Override
			public void storeCommunityNormObject(String communityId, JsonObject community,
					Handler<AsyncResult<JsonObject>> storeHandler) {

				storeHandler.handle(Future.failedFuture(cause));
			}

		};

		repository.storeCommunityNorm("communityId", new CommunityNorm(),
				testContext.failing(fail -> testContext.verify(() -> {
					assertThat(fail).isEqualTo(cause);
					testContext.completeNow();
				})));

	}

}
