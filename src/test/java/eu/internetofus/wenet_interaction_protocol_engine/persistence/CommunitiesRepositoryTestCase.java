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

import eu.internetofus.wenet_interaction_protocol_engine.api.communities.Community;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.CommunityTest;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;

/**
 * Generic test over the {@link CommunitiesRepository}.
 *
 * @param <T> the repository to test.
 *
 * @see CommunitiesRepository
 *
 * @author UDT-IA, IIIA-CSIC
 */
public abstract class CommunitiesRepositoryTestCase<T extends CommunitiesRepository> {

	/**
	 * The repository to do the tests.
	 */
	protected T repository;

	/**
	 * Verify that can not found a community if it is not defined.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunity(String, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotFoundUndefinedCommunity(VertxTestContext testContext) {

		this.repository.searchCommunity("undefined community identifier", testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not found a community object if it is not defined.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityObject(String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotFoundUndefinedCommunityObject(VertxTestContext testContext) {

		this.repository.searchCommunityObject("undefined community identifier", testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can found a community.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunity(String, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunity(VertxTestContext testContext) {

		this.repository.storeCommunity(new Community(), testContext.succeeding(storedCommunity -> {

			this.repository.searchCommunity(storedCommunity._id,
					testContext.succeeding(foundCommunity -> testContext.verify(() -> {
						assertThat(foundCommunity).isEqualTo(storedCommunity);
						testContext.completeNow();
					})));

		}));

	}

	/**
	 * Verify that can found a community object.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityObject(String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityObject(VertxTestContext testContext) {

		this.repository.storeCommunity(new JsonObject(), testContext.succeeding(storedCommunity -> {

			this.repository.searchCommunityObject(storedCommunity.getString("_id"),
					testContext.succeeding(foundCommunity -> testContext.verify(() -> {
						assertThat(foundCommunity).isEqualTo(storedCommunity);
						testContext.completeNow();
					})));

		}));

	}

	/**
	 * Verify that can not store a community that can not be an object.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunity(Community, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotStoreACommunityThatCanNotBeAnObject(VertxTestContext testContext) {

		final Community community = new Community() {

			/**
			 * {@inheritDoc}
			 */
			@Override
			public JsonObject toJsonObject() {

				return null;
			}
		};
		community._id = "undefined community identifier";
		this.repository.storeCommunity(community, testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can store a community.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunity(String, io.vertx.core.Handler)
	 */
	@Test
	public void shouldStoreCommunity(VertxTestContext testContext) {

		final Community community = new Community();
		this.repository.storeCommunity(community, testContext.succeeding(storedCommunity -> testContext.verify(() -> {

			assertThat(storedCommunity).isNotNull();
			assertThat(storedCommunity._id).isNotEmpty();
			testContext.completeNow();
		})));

	}

	/**
	 * Verify that can store a community object.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunity(JsonObject, io.vertx.core.Handler)
	 */
	@Test
	public void shouldStoreCommunityObject(VertxTestContext testContext) {

		final long now = System.currentTimeMillis();
		this.repository.storeCommunity(new JsonObject(),
				testContext.succeeding(storedCommunity -> testContext.verify(() -> {

					assertThat(storedCommunity).isNotNull();
					final String id = storedCommunity.getString("_id");
					assertThat(id).isNotEmpty();
					assertThat(storedCommunity.getLong("sinceTime", 0l)).isNotEqualTo(0).isGreaterThanOrEqualTo(now);
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can not update a community if it is not defined.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunity(Community, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateUndefinedCommunity(VertxTestContext testContext) {

		final Community community = new Community();
		community._id = "undefined community identifier";
		this.repository.updateCommunity(community, testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not update a community if it is not defined.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunity(JsonObject, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateUndefinedCommunityObject(VertxTestContext testContext) {

		final JsonObject community = new JsonObject().put("_id", "undefined community identifier");
		this.repository.updateCommunity(community, testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not update a community if it is not defined.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunity(Community, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateACommunityThatCanNotBeAnObject(VertxTestContext testContext) {

		final Community community = new Community() {

			/**
			 * {@inheritDoc}
			 */
			@Override
			public JsonObject toJsonObject() {

				return null;
			}
		};
		community._id = "undefined community identifier";
		this.repository.updateCommunity(community, testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can update a community.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunity(Community, io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdateCommunity(VertxTestContext testContext) {

		final Community community = new Community();

		this.repository.storeCommunity(community, testContext.succeeding(stored -> testContext.verify(() -> {

			final Community update = new CommunityTest().createModelExample(23);
			update._id = stored._id;
			this.repository.updateCommunity(update, testContext.succeeding(updatedCommunity -> testContext.verify(() -> {

				assertThat(updatedCommunity).isNotNull();
				assertThat(updatedCommunity._id).isNotEmpty().isEqualTo(stored._id);
				assertThat(updatedCommunity).isEqualTo(update);
				this.repository.searchCommunity(stored._id, testContext.succeeding(foundCommunity -> testContext.verify(() -> {
					assertThat(foundCommunity).isEqualTo(updatedCommunity);
					testContext.completeNow();
				})));
			})));

		})));

	}

	/**
	 * Verify that update a defined community object.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunity(JsonObject, io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdateCommunityObject(VertxTestContext testContext) {

		this.repository.storeCommunity(new JsonObject().put("name", "Community name"),
				testContext.succeeding(stored -> testContext.verify(() -> {

					final String id = stored.getString("_id");
					final JsonObject update = new JsonObject().put("_id", id).put("name", "New community name");
					this.repository.updateCommunity(update, testContext.succeeding(updatedCommunity -> testContext.verify(() -> {

						update.put("sinceTime", stored.getLong("sinceTime"));
						assertThat(updatedCommunity).isEqualTo(update);
						this.repository.searchCommunityObject(id,
								testContext.succeeding(foundCommunity -> testContext.verify(() -> {
									stored.put("name", "New community name");
									assertThat(foundCommunity).isEqualTo(stored);
									testContext.completeNow();
								})));
					})));

				})));

	}

	/**
	 * Verify that can not delete a community if it is not defined.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunity(String, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotDeleteUndefinedCommunity(VertxTestContext testContext) {

		this.repository.deleteCommunity("undefined community identifier", testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can delete a community.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunity(JsonObject, io.vertx.core.Handler)
	 */
	@Test
	public void shouldDeleteCommunity(VertxTestContext testContext) {

		this.repository.storeCommunity(new JsonObject(), testContext.succeeding(stored -> {

			final String id = stored.getString("_id");
			this.repository.deleteCommunity(id, testContext.succeeding(success -> {

				this.repository.searchCommunityObject(id, testContext.failing(search -> {

					testContext.completeNow();

				}));

			}));

		}));

	}

}
