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

import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.common.TimeManager;
import eu.internetofus.common.components.Model;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.PublishedNorm;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.PublishedNormTest;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;

/**
 * Integration test over the {@link NormsRepository}.
 *
 * @see NormsRepository
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class NormsRepositoryIT {

	/**
	 * Verify that can not found a published norm if it is not defined.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#searchPublishedNorm(String, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotFoundUndefinedPublishedNorm(Vertx vertx, VertxTestContext testContext) {

		NormsRepository.createProxy(vertx).searchPublishedNorm("undefined norm identifier", testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not found a published norm object if it is not defined.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#searchPublishedNormObject(String, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotFoundUndefinedPublishedNormObject(Vertx vertx, VertxTestContext testContext) {

		NormsRepository.createProxy(vertx).searchPublishedNormObject("undefined norm identifier",
				testContext.failing(failed -> {
					testContext.completeNow();
				}));

	}

	/**
	 * Verify that can found a published norm.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#searchPublishedNorm(String, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundPublishedNorm(Vertx vertx, VertxTestContext testContext) {

		NormsRepository.createProxy(vertx).storePublishedNorm(new PublishedNorm(), testContext.succeeding(storedNorm -> {

			NormsRepository.createProxy(vertx).searchPublishedNorm(storedNorm.id,
					testContext.succeeding(foundNorm -> testContext.verify(() -> {
						assertThat(foundNorm).isEqualTo(storedNorm);
						testContext.completeNow();
					})));

		}));

	}

	/**
	 * Verify that can found a published norm object.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#searchPublishedNormObject(String, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundPublishedNormObject(Vertx vertx, VertxTestContext testContext) {

		NormsRepository.createProxy(vertx).storePublishedNorm(new JsonObject(), testContext.succeeding(storedNorm -> {

			NormsRepository.createProxy(vertx).searchPublishedNormObject(storedNorm.getString("id"),
					testContext.succeeding(foundNorm -> testContext.verify(() -> {
						assertThat(foundNorm).isEqualTo(storedNorm);
						testContext.completeNow();
					})));

		}));

	}

	/**
	 * Verify that can not store a published norm that can not be an object.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#updatePublishedNorm(PublishedNorm,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotStoreANormThatCanNotBeAnObject(Vertx vertx, VertxTestContext testContext) {

		final PublishedNorm norm = new PublishedNorm() {

			/**
			 * {@inheritDoc}
			 */
			@Override
			public JsonObject toJsonObject() {

				return null;
			}
		};
		norm.id = "undefined norm identifier";
		NormsRepository.createProxy(vertx).storePublishedNorm(norm, testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can store a published norm.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#searchPublishedNorm(String, io.vertx.core.Handler)
	 */
	@Test
	public void shouldStorePublishedNorm(Vertx vertx, VertxTestContext testContext) {

		final PublishedNorm norm = new PublishedNorm();
		final long now = TimeManager.now();
		NormsRepository.createProxy(vertx).storePublishedNorm(norm,
				testContext.succeeding(storedNorm -> testContext.verify(() -> {

					assertThat(storedNorm).isNotNull();
					assertThat(storedNorm.id).isNotEmpty();
					assertThat(storedNorm._creationTs).isGreaterThanOrEqualTo(now);
					assertThat(storedNorm._lastUpdateTs).isGreaterThanOrEqualTo(now);
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can store a published norm object.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#storePublishedNorm(JsonObject, io.vertx.core.Handler)
	 */
	@Test
	public void shouldStorePublishedNormObject(Vertx vertx, VertxTestContext testContext) {

		final long now = TimeManager.now();
		NormsRepository.createProxy(vertx).storePublishedNorm(new JsonObject(),
				testContext.succeeding(storedNorm -> testContext.verify(() -> {

					assertThat(storedNorm).isNotNull();
					final String id = storedNorm.getString("id");
					assertThat(id).isNotEmpty();
					assertThat(storedNorm.getLong("_creationTs", 0l)).isNotEqualTo(0).isGreaterThanOrEqualTo(now);
					assertThat(storedNorm.getLong("_lastUpdateTs", 0l)).isNotEqualTo(0).isGreaterThanOrEqualTo(now);
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can not update a published norm if it is not defined.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#updatePublishedNorm(PublishedNorm,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateUndefinedPublishedNorm(Vertx vertx, VertxTestContext testContext) {

		final PublishedNorm norm = new PublishedNorm();
		norm.id = "undefined norm identifier";
		NormsRepository.createProxy(vertx).updatePublishedNorm(norm, testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not update a published norm if it is not defined.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateUndefinedPublishedNormObject(Vertx vertx, VertxTestContext testContext) {

		final JsonObject norm = new JsonObject().put("id", "undefined norm identifier");
		NormsRepository.createProxy(vertx).updatePublishedNorm(norm, testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not update a published norm if it is not defined.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#updatePublishedNorm(PublishedNorm,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateANormThatCanNotBeAnObject(Vertx vertx, VertxTestContext testContext) {

		final PublishedNorm norm = new PublishedNorm() {

			/**
			 * {@inheritDoc}
			 */
			@Override
			public JsonObject toJsonObject() {

				return null;
			}
		};
		norm.id = "undefined norm identifier";
		NormsRepository.createProxy(vertx).updatePublishedNorm(norm, testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can update a published norm.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#updatePublishedNorm(PublishedNorm,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdatePublishedNorm(Vertx vertx, VertxTestContext testContext) {

		final PublishedNorm norm = new PublishedNorm();

		NormsRepository.createProxy(vertx).storePublishedNorm(norm,
				testContext.succeeding(stored -> testContext.verify(() -> {

					final PublishedNorm update = new PublishedNormTest().createModelExample(23);
					update.id = stored.id;
					Thread.sleep(1000);
					final long now = TimeManager.now();
					NormsRepository.createProxy(vertx).updatePublishedNorm(update, testContext.succeeding(empty -> {

						NormsRepository.createProxy(vertx).searchPublishedNorm(stored.id,
								testContext.succeeding(foundNorm -> testContext.verify(() -> {

									assertThat(foundNorm._creationTs).isEqualTo(stored._creationTs);
									assertThat(foundNorm._lastUpdateTs).isGreaterThanOrEqualTo(now);
									update._creationTs = stored._creationTs;
									update._lastUpdateTs = foundNorm._lastUpdateTs;
									assertThat(foundNorm).isEqualTo(update);
									testContext.completeNow();
								})));
					}));

				})));

	}

	/**
	 * Verify that update a defined published norm object.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdatePublishedNormObject(Vertx vertx, VertxTestContext testContext) {

		NormsRepository.createProxy(vertx).storePublishedNorm(new JsonObject().put("name", "Norm name"),
				testContext.succeeding(stored -> testContext.verify(() -> {

					final String id = stored.getString("id");
					final JsonObject update = new JsonObject().put("id", id).put("name", "New norm name");
					NormsRepository.createProxy(vertx).updatePublishedNorm(update,
							testContext.succeeding(empty -> testContext.verify(() -> {

								NormsRepository.createProxy(vertx).searchPublishedNormObject(id,
										testContext.succeeding(foundNorm -> testContext.verify(() -> {
											stored.put("name", "New norm name");
											assertThat(foundNorm).isEqualTo(stored);
											testContext.completeNow();
										})));
							})));

				})));

	}

	/**
	 * Verify that can not delete a published norm if it is not defined.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#searchPublishedNorm(String, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotDeleteUndefinedPublishedNorm(Vertx vertx, VertxTestContext testContext) {

		NormsRepository.createProxy(vertx).deletePublishedNorm("undefined norm identifier", testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can delete a published norm.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
	 */
	@Test
	public void shouldDeletePublishedNorm(Vertx vertx, VertxTestContext testContext) {

		NormsRepository.createProxy(vertx).storePublishedNorm(new JsonObject(), testContext.succeeding(stored -> {

			final String id = stored.getString("id");
			NormsRepository.createProxy(vertx).deletePublishedNorm(id, testContext.succeeding(success -> {

				NormsRepository.createProxy(vertx).searchPublishedNormObject(id, testContext.failing(search -> {

					testContext.completeNow();

				}));

			}));

		}));

	}

	/**
	 * Verify that can obtain the published norms.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#searchPublishedNormsPageObject(JsonObject, int, int,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shoulFoundPublishedNormPage(Vertx vertx, VertxTestContext testContext) {

		final NormsRepository repository = NormsRepository.createProxy(vertx);
		final String name = UUID.randomUUID().toString();
		repository.searchPublishedNormsPageObject(name, null, null, null, null, null, 0, 10,
				testContext.succeeding(found -> testContext.verify(() -> {

					assertThat(found.getLong("total")).isEqualTo(0l);
					assertThat(found.getJsonArray("norms", null)).isNull();
					final PublishedNorm norm1 = new PublishedNormTest().createModelExample(1);
					norm1.name = name;
					repository.storePublishedNorm(norm1, testContext.succeeding(stored1 -> testContext.verify(() -> {
						repository.searchPublishedNormsPageObject(name, null, null, null, null, null, 0, 10,
								testContext.succeeding(found2 -> testContext.verify(() -> {
									assertThat(found.getLong("total")).isEqualTo(1l);
									assertThat(found.getJsonArray("norms", null)).isNotNull();
									assertThat(Model.fromJsonObject(found.getJsonArray("norms").getJsonObject(0), PublishedNorm.class))
											.isEqualTo(norm1);

									testContext.completeNow();
								})));
					})));

				})));

	}

}
