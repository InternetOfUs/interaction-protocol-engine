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

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Semaphore;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

import eu.internetofus.common.TimeManager;
import eu.internetofus.common.api.models.Model;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.PublishedNorm;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.PublishedNormTest;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.PublishedNormsPage;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
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

			NormsRepository.createProxy(vertx).searchPublishedNormObject(storedNorm.getString("_id"),
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
		NormsRepository.createProxy(vertx).storePublishedNorm(norm,
				testContext.succeeding(storedNorm -> testContext.verify(() -> {

					assertThat(storedNorm).isNotNull();
					assertThat(storedNorm.id).isNotEmpty();
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
					final String id = storedNorm.getString("_id");
					assertThat(id).isNotEmpty();
					assertThat(storedNorm.getLong("publishTime", 0l)).isNotEqualTo(0).isGreaterThanOrEqualTo(now);
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

		final JsonObject norm = new JsonObject().put("_id", "undefined norm identifier");
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
					NormsRepository.createProxy(vertx).updatePublishedNorm(update,
							testContext.succeeding(empty -> testContext.verify(() -> {

								NormsRepository.createProxy(vertx).searchPublishedNorm(stored.id,
										testContext.succeeding(foundNorm -> testContext.verify(() -> {
											assertThat(foundNorm).isEqualTo(update);
											testContext.completeNow();
										})));
							})));

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

					final String id = stored.getString("_id");
					final JsonObject update = new JsonObject().put("_id", id).put("name", "New norm name");
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

			final String id = stored.getString("_id");
			NormsRepository.createProxy(vertx).deletePublishedNorm(id, testContext.succeeding(success -> {

				NormsRepository.createProxy(vertx).searchPublishedNormObject(id, testContext.failing(search -> {

					testContext.completeNow();

				}));

			}));

		}));

	}

	/**
	 * Remove all the published norms defined on the
	 * NormsRepository.createProxy(vertx).
	 *
	 * @param pool that create the mongo connections.
	 */
	public static final void removeAllNorms(MongoClient pool) {

		final Semaphore semaphore = new Semaphore(0);
		pool.removeDocuments(NormsRepositoryImpl.PUBLISHED_NORMS_COLLECTION, new JsonObject(), remove -> {

			semaphore.release();
		});

		try {
			semaphore.acquire();
		} catch (final InterruptedException ignored) {
		}

	}

	// /**
	// * Remove all the published norms defined on the
	// NormsRepository.createProxy(vertx).
	// *
	// * @param repository to use.
	// * @param max number of norms to try to create.
	// *
	// * @return the published norms that has been created.
	// */
	// public static final <T extends NormsRepository> List<PublishedNorm>
	// createAndStoreSomePublishedNorms(T repository,
	// int max) {
	//
	// final List<PublishedNorm> norms = new ArrayList<>();
	// final Semaphore semaphore = new Semaphore(0);
	// createNextPublishedNorm(repository, norms, max, semaphore);
	//
	// try {
	// semaphore.acquire(max);
	// } catch (final InterruptedException ignored) {
	// }
	//
	// return norms;
	// }

	// /**
	// * Create an store a published norm.
	// *
	// * @param repository to use.
	// * @param norms that has been created.
	// * @param tries number maximum of times to create a published norm.
	// * @param semaphore to inform when the norm is created.
	// */
	// private static <T extends NormsRepository> void createNextPublishedNorm(T
	// repository, List<PublishedNorm> norms,
	// int tries, Semaphore semaphore) {
	//
	// final PublishedNorm norm = new
	// PublishedNormTest().createModelExample(norms.size());
	// NormsRepository.createProxy(vertx).storePublishedNorm(norm, stored -> {
	// if (!stored.failed()) {
	//
	// norms.add(stored.result());
	//
	// }
	// if (tries > 1) {
	// createNextPublishedNorm(repository, norms, tries - 1, semaphore);
	// }
	// semaphore.release();
	// });
	//
	// }

	// /**
	// * Verify that can find all the published norms.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindAllNorms(MongoClient pool, Vertx vertx,
	// VertxTestContext testContext) {
	//
	// removeAllNorms(pool);
	// final List<PublishedNorm> norms =
	// createAndStoreSomePublishedNorms(repository, 23);
	//
	// final String name = null;
	// final String description = null;
	// final List<String> keywords = null;
	// final String publishedId = null;
	// final Long publishFrom = null;
	// final Long publishTo = null;
	// final int offset = 0;
	// final int limit = 100;
	// NormsRepository.createProxy(vertx).searchPublishedNormsPageObject(name,
	// description, keywords, publishedId, publishFrom, publishTo, offset,
	// limit, testContext.succeeding(found -> testContext.verify(() -> {
	//
	// final PublishedNormsPage foundPage = Model.fromJsonObject(found,
	// PublishedNormsPage.class);
	// assertThat(foundPage.offset).isEqualTo(offset);
	// assertThat(foundPage.total).isEqualTo(23);
	// assertThat(foundPage.norms).isEqualTo(norms);
	//
	// testContext.completeNow();
	// })));
	//
	// }

	// /**
	// * Verify that can find some norms on a range.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindPublishedNormsInARange(MongoClient pool, Vertx vertx,
	// VertxTestContext testContext) {
	//
	// removeAllNorms(pool);
	// final List<PublishedNorm> norms =
	// createAndStoreSomePublishedNorms(repository, 23);
	//
	// final String name = null;
	// final String description = null;
	// final List<String> keywords = null;
	// final String publishedId = null;
	// final Long publishFrom = null;
	// final Long publishTo = null;
	// final int offset = 5;
	// final int limit = 10;
	// NormsRepository.createProxy(vertx).searchPublishedNormsPageObject(name,
	// description, keywords, publishedId, publishFrom, publishTo, offset,
	// limit, testContext.succeeding(found -> testContext.verify(() -> {
	//
	// final PublishedNormsPage foundPage = Model.fromJsonObject(found,
	// PublishedNormsPage.class);
	// assertThat(foundPage.offset).isEqualTo(offset);
	// assertThat(foundPage.total).isEqualTo(23);
	// assertThat(foundPage.norms).isEqualTo(norms.subList(5, 15));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that can find some norms with a specific name.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindPublishedNormsWithAName(MongoClient pool, Vertx vertx,
	// VertxTestContext testContext) {
	//
	// removeAllNorms(pool);
	// final List<PublishedNorm> norms =
	// createAndStoreSomePublishedNorms(repository, 23);
	//
	// final String name = ".+1\\d";
	// final String description = null;
	// final List<String> keywords = null;
	// final String publishedId = null;
	// final Long publishFrom = null;
	// final Long publishTo = null;
	// final int offset = 0;
	// final int limit = 100;
	// NormsRepository.createProxy(vertx).searchPublishedNormsPageObject(name,
	// description, keywords, publishedId, publishFrom, publishTo, offset,
	// limit, testContext.succeeding(found -> testContext.verify(() -> {
	//
	// final PublishedNormsPage foundPage = Model.fromJsonObject(found,
	// PublishedNormsPage.class);
	// assertThat(foundPage.offset).isEqualTo(0);
	// assertThat(foundPage.total).isEqualTo(10);
	// assertThat(foundPage.norms).isEqualTo(norms.subList(10, 20));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that can find some norms with a specific description.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindPublishedNormsWithADescription(MongoClient pool, Vertx
	// vertx,
	// VertxTestContext testContext) {
	//
	// removeAllNorms(pool);
	// final List<PublishedNorm> norms =
	// createAndStoreSomePublishedNorms(repository, 23);
	//
	// final String name = null;
	// final String description = ".+2\\d";
	// final List<String> keywords = null;
	// final String publishedId = null;
	// final Long publishFrom = null;
	// final Long publishTo = null;
	// final int offset = 0;
	// final int limit = 100;
	// NormsRepository.createProxy(vertx).searchPublishedNormsPageObject(name,
	// description, keywords, publishedId, publishFrom, publishTo, offset,
	// limit, testContext.succeeding(found -> testContext.verify(() -> {
	//
	// final PublishedNormsPage foundPage = Model.fromJsonObject(found,
	// PublishedNormsPage.class);
	// assertThat(foundPage.offset).isEqualTo(0);
	// assertThat(foundPage.total).isEqualTo(3);
	// assertThat(foundPage.norms).isEqualTo(norms.subList(20, 23));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that can find some norms with a specific keywords.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindPublishedNormsWithKeywords(MongoClient pool, Vertx
	// vertx,
	// VertxTestContext testContext) {
	//
	// removeAllNorms(pool);
	// final List<PublishedNorm> norms =
	// createAndStoreSomePublishedNorms(repository, 23);
	//
	// final String name = null;
	// final String description = null;
	// final List<String> keywords = new ArrayList<>();
	// keywords.add("keyword 19");
	// keywords.add("keyword 21");
	// final String publishedId = null;
	// final Long publishFrom = null;
	// final Long publishTo = null;
	// final int offset = 0;
	// final int limit = 100;
	// NormsRepository.createProxy(vertx).searchPublishedNormsPageObject(name,
	// description, keywords, publishedId, publishFrom, publishTo, offset,
	// limit, testContext.succeeding(found -> testContext.verify(() -> {
	//
	// final PublishedNormsPage foundPage = Model.fromJsonObject(found,
	// PublishedNormsPage.class);
	// assertThat(foundPage.offset).isEqualTo(0);
	// assertThat(foundPage.total).isEqualTo(2);
	// assertThat(foundPage.norms).isEqualTo(norms.subList(20, 22));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that can find some norms with a one keyword.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindPublishedNormsWithKeyword(MongoClient pool, Vertx
	// vertx,
	// VertxTestContext testContext) {
	//
	// removeAllNorms(pool);
	// final List<PublishedNorm> norms =
	// createAndStoreSomePublishedNorms(repository, 23);
	//
	// final String name = null;
	// final String description = null;
	// final List<String> keywords = new ArrayList<>();
	// keywords.add("keyword 19");
	// final String publishedId = null;
	// final Long publishFrom = null;
	// final Long publishTo = null;
	// final int offset = 0;
	// final int limit = 100;
	// NormsRepository.createProxy(vertx).searchPublishedNormsPageObject(name,
	// description, keywords, publishedId, publishFrom, publishTo, offset,
	// limit, testContext.succeeding(found -> testContext.verify(() -> {
	//
	// final PublishedNormsPage foundPage = Model.fromJsonObject(found,
	// PublishedNormsPage.class);
	// assertThat(foundPage.offset).isEqualTo(0);
	// assertThat(foundPage.total).isEqualTo(4);
	// assertThat(foundPage.norms).isEqualTo(norms.subList(18, 22));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that can find some norms with a specific publishedId.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindPublishedNormsWithAnPublishedId(MongoClient pool, Vertx
	// vertx,
	// VertxTestContext testContext) {
	//
	// removeAllNorms(pool);
	// final List<PublishedNorm> norms =
	// createAndStoreSomePublishedNorms(repository, 23);
	//
	// final String name = null;
	// final String description = null;
	// final List<String> keywords = null;
	// final String publishedId = ".+1\\d";
	// final Long publishFrom = null;
	// final Long publishTo = null;
	// final int offset = 0;
	// final int limit = 100;
	// NormsRepository.createProxy(vertx).searchPublishedNormsPageObject(name,
	// description, keywords, publishedId, publishFrom, publishTo, offset,
	// limit, testContext.succeeding(found -> testContext.verify(() -> {
	//
	// final PublishedNormsPage foundPage = Model.fromJsonObject(found,
	// PublishedNormsPage.class);
	// assertThat(foundPage.offset).isEqualTo(0);
	// assertThat(foundPage.total).isEqualTo(10);
	// assertThat(foundPage.norms).isEqualTo(norms.subList(10, 20));
	//
	// testContext.completeNow();
	// })));
	//
	// }

	/**
	 * Create an aggregate some norms with a fake {@link PublishedNorm#publishTime}.
	 *
	 * @param pool that create the mongo connections.
	 * @param max  number of norms to try to create.
	 *
	 * @return the aggregated norms.
	 */
	public static List<PublishedNorm> createAndStoreSomeFakePublishNorms(MongoClient pool, int max) {

		final List<PublishedNorm> norms = new ArrayList<>();
		final Semaphore semaphore = new Semaphore(0);
		createNextFakePublishNorm(pool, norms, max, semaphore);

		try {
			semaphore.acquire(max);
		} catch (final InterruptedException ignored) {
		}

		return norms;

	}

	/**
	 * Create an store a published norm with a fake publish time.
	 *
	 * @param pool      that create the mongo connections.
	 * @param norms     that has been created.
	 * @param tries     number maximum of times to create a published norm.
	 * @param semaphore to inform when the norm is created.
	 */
	private static void createNextFakePublishNorm(MongoClient pool, List<PublishedNorm> norms, int tries,
			Semaphore semaphore) {

		final int index = norms.size();
		final PublishedNorm norm = new PublishedNormTest().createModelExample(index);
		norm.publishTime = index * 100000;
		pool.save(NormsRepositoryImpl.PUBLISHED_NORMS_COLLECTION, norm.toJsonObject(), stored -> {
			if (!stored.failed()) {

				norm.id = stored.result();
				norms.add(norm);

			}
			if (tries > 1) {
				createNextFakePublishNorm(pool, norms, tries - 1, semaphore);
			}
			semaphore.release();
		});

	}

	/**
	 * Verify that can find some norms publish a from time.
	 *
	 * @param pool        that create the mongo connections.
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindPublishedNormsPublishAFromTime(MongoClient pool, Vertx vertx, VertxTestContext testContext) {

		removeAllNorms(pool);
		final List<PublishedNorm> norms = createAndStoreSomeFakePublishNorms(pool, 23);

		final String name = null;
		final String description = null;
		final List<String> keywords = null;
		final String publishedId = null;
		final Long publishFrom = 1500000l;
		final Long publishTo = null;
		final int offset = 0;
		final int limit = 100;
		NormsRepository.createProxy(vertx).searchPublishedNormsPageObject(name, description, keywords, publishedId,
				publishFrom, publishTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final PublishedNormsPage foundPage = Model.fromJsonObject(found, PublishedNormsPage.class);
					assertThat(foundPage.offset).isEqualTo(0);
					assertThat(foundPage.total).isEqualTo(8);
					assertThat(foundPage.norms).isEqualTo(norms.subList(15, 23));

					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can find some norms publish a to time.
	 *
	 * @param pool        that create the mongo connections.
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindPublishedNormsPublishAToTime(MongoClient pool, Vertx vertx, VertxTestContext testContext) {

		removeAllNorms(pool);
		final List<PublishedNorm> norms = createAndStoreSomeFakePublishNorms(pool, 23);

		final String name = null;
		final String description = null;
		final List<String> keywords = null;
		final String publishedId = null;
		final Long publishFrom = null;
		final Long publishTo = 1500000l;
		final int offset = 0;
		final int limit = 100;
		NormsRepository.createProxy(vertx).searchPublishedNormsPageObject(name, description, keywords, publishedId,
				publishFrom, publishTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final PublishedNormsPage foundPage = Model.fromJsonObject(found, PublishedNormsPage.class);
					assertThat(foundPage.offset).isEqualTo(0);
					assertThat(foundPage.total).isEqualTo(16);
					assertThat(foundPage.norms).isEqualTo(norms.subList(0, 16));

					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can find some norms publish time in a range.
	 *
	 * @param pool        that create the mongo connections.
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindPublishedNormsPublishTimeInARange(MongoClient pool, Vertx vertx, VertxTestContext testContext) {

		removeAllNorms(pool);
		final List<PublishedNorm> norms = createAndStoreSomeFakePublishNorms(pool, 23);

		final String name = null;
		final String description = null;
		final List<String> keywords = null;
		final String publishedId = null;
		final Long publishFrom = 1200000l;
		final Long publishTo = 1800000l;
		final int offset = 0;
		final int limit = 100;
		NormsRepository.createProxy(vertx).searchPublishedNormsPageObject(name, description, keywords, publishedId,
				publishFrom, publishTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final PublishedNormsPage foundPage = Model.fromJsonObject(found, PublishedNormsPage.class);
					assertThat(foundPage.offset).isEqualTo(0);
					assertThat(foundPage.total).isEqualTo(7);
					assertThat(foundPage.norms).isEqualTo(norms.subList(12, 19));

					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can find some norms.
	 *
	 * @param pool        that create the mongo connections.
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#updatePublishedNorm(JsonObject, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindPublishedNorms(MongoClient pool, Vertx vertx, VertxTestContext testContext) {

		removeAllNorms(pool);
		final List<PublishedNorm> norms = createAndStoreSomeFakePublishNorms(pool, 23);

		final String name = ".+ 1\\d";
		final String description = "3|4|5|6";
		final List<String> keywords = new ArrayList<>();
		keywords.add("\\d{2}");
		final String publishedId = ".*";
		final Long publishFrom = 1200000l;
		final Long publishTo = 1800000l;
		final int offset = 1;
		final int limit = 2;
		NormsRepository.createProxy(vertx).searchPublishedNormsPageObject(name, description, keywords, publishedId,
				publishFrom, publishTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final PublishedNormsPage foundPage = Model.fromJsonObject(found, PublishedNormsPage.class);
					assertThat(foundPage.offset).isEqualTo(1);
					assertThat(foundPage.total).isEqualTo(4);
					assertThat(foundPage.norms).isEqualTo(norms.subList(14, 16));

					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can find an empty published norms page if any norm match.
	 *
	 * @param pool        that create the mongo connections.
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see NormsRepository#searchPublishedNormsPageObject(JsonObject, int, int,
	 *      io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindEmptyPublishedNormsPageIfAnyMatch(MongoClient pool, Vertx vertx, VertxTestContext testContext) {

		removeAllNorms(pool);
		final String name = null;
		final String description = null;
		final List<String> keywords = new ArrayList<>();
		final String publishedId = null;
		final Long publishFrom = null;
		final Long publishTo = null;
		final int offset = 0;
		final int limit = 100;
		NormsRepository.createProxy(vertx).searchPublishedNormsPageObject(name, description, keywords, publishedId,
				publishFrom, publishTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final PublishedNormsPage foundPage = Model.fromJsonObject(found, PublishedNormsPage.class);
					assertThat(foundPage.offset).isEqualTo(0);
					assertThat(foundPage.total).isEqualTo(0);
					assertThat(foundPage.norms).isNull();

					testContext.completeNow();
				})));

	}
	//
	// /**
	// * Verify that can find an empty published norms page if any norm match.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see NormsRepository#searchPublishedNormsPageObject(JsonObject, int, int,
	// * io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindEmptyPublishedNormsPageIfOffsetIsOutOfRange(MongoClient
	// pool, Vertx vertx,
	// VertxTestContext testContext) {
	//
	// removeAllNorms(pool);
	// createAndStoreSomePublishedNorms(repository, 2);
	// final String name = null;
	// final String description = null;
	// final List<String> keywords = new ArrayList<>();
	// final String publishedId = null;
	// final Long publishFrom = null;
	// final Long publishTo = null;
	// final int offset = 3;
	// final int limit = 100;
	// NormsRepository.createProxy(vertx).searchPublishedNormsPageObject(name,
	// description, keywords, publishedId, publishFrom, publishTo, offset,
	// limit, testContext.succeeding(found -> testContext.verify(() -> {
	//
	// final PublishedNormsPage foundPage = Model.fromJsonObject(found,
	// PublishedNormsPage.class);
	// assertThat(foundPage.offset).isEqualTo(3);
	// assertThat(foundPage.total).isEqualTo(2);
	// assertThat(foundPage.norms).isNull();
	//
	// testContext.completeNow();
	// })));
	//
	// }

}
