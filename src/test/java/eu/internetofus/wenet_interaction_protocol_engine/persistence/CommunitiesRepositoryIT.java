/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to
 * do so, subject to the following conditions:
 *
 * The above copyright notice and permission notice shall be included in all
 * copies or substantial portions of the Software.
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
import java.util.concurrent.Semaphore;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.common.TimeManager;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.Community;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.CommunityMember;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.CommunityNorm;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.CommunityTest;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.junit5.VertxTestContext;

/**
 * Integration test over the {@link CommunitiesRepository}.
 *
 * @see CommunitiesRepository
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class CommunitiesRepositoryIT {

	/**
	 * Verify that can not found a community if it is not defined.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunity(String, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotFoundUndefinedCommunity(Vertx vertx, VertxTestContext testContext) {

		CommunitiesRepository.createProxy(vertx).searchCommunity("undefined community identifier",
				testContext.failing(failed -> {
					testContext.completeNow();
				}));

	}

	/**
	 * Verify that can not found a community object if it is not defined.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityObject(String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotFoundUndefinedCommunityObject(Vertx vertx, VertxTestContext testContext) {

		CommunitiesRepository.createProxy(vertx).searchCommunityObject("undefined community identifier",
				testContext.failing(failed -> {
					testContext.completeNow();
				}));

	}

	/**
	 * Verify that can found a community.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunity(String, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunity(Vertx vertx, VertxTestContext testContext) {

		CommunitiesRepository.createProxy(vertx).storeCommunity(new Community(), testContext.succeeding(storedCommunity -> {

			CommunitiesRepository.createProxy(vertx).searchCommunity(storedCommunity.id,
					testContext.succeeding(foundCommunity -> testContext.verify(() -> {
						assertThat(foundCommunity).isEqualTo(storedCommunity);
						testContext.completeNow();
					})));

		}));

	}

	/**
	 * Verify that can found a community object.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityObject(String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityObject(Vertx vertx, VertxTestContext testContext) {

		CommunitiesRepository.createProxy(vertx).storeCommunity(new JsonObject(),
				testContext.succeeding(storedCommunity -> {

					CommunitiesRepository.createProxy(vertx).searchCommunityObject(storedCommunity.getString("id"),
							testContext.succeeding(foundCommunity -> testContext.verify(() -> {
								assertThat(foundCommunity).isEqualTo(storedCommunity);
								testContext.completeNow();
							})));

				}));

	}

	/**
	 * Verify that can not store a community that can not be an object.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunity(Community, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotStoreACommunityThatCanNotBeAnObject(Vertx vertx, VertxTestContext testContext) {

		final Community community = new Community() {

			/**
			 * {@inheritDoc}
			 */
			@Override
			public JsonObject toJsonObject() {

				return null;
			}
		};
		community.id = "undefined community identifier";
		CommunitiesRepository.createProxy(vertx).storeCommunity(community, testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can store a community.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunity(Community, io.vertx.core.Handler)
	 */
	@Test
	public void shouldStoreCommunity(Vertx vertx, VertxTestContext testContext) {

		final Community community = new Community();
		CommunitiesRepository.createProxy(vertx).storeCommunity(community,
				testContext.succeeding(storedCommunity -> testContext.verify(() -> {

					assertThat(storedCommunity).isNotNull();
					assertThat(storedCommunity.id).isNotEmpty();
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can store a community object.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunity(JsonObject, io.vertx.core.Handler)
	 */
	@Test
	public void shouldStoreCommunityObject(Vertx vertx, VertxTestContext testContext) {

		final long now = TimeManager.now();
		CommunitiesRepository.createProxy(vertx).storeCommunity(new JsonObject(),
				testContext.succeeding(storedCommunity -> testContext.verify(() -> {

					assertThat(storedCommunity).isNotNull();
					final String id = storedCommunity.getString("id");
					assertThat(id).isNotEmpty();
					assertThat(storedCommunity.getLong("_creationTs", 0l)).isNotEqualTo(0).isGreaterThanOrEqualTo(now);
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can not update a community if it is not defined.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunity(Community, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateUndefinedCommunity(Vertx vertx, VertxTestContext testContext) {

		final Community community = new Community();
		community.id = "undefined community identifier";
		CommunitiesRepository.createProxy(vertx).updateCommunity(community, testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not update a community if it is not defined.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunity(JsonObject, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateUndefinedCommunityObject(Vertx vertx, VertxTestContext testContext) {

		final JsonObject community = new JsonObject().put("id", "undefined community identifier");
		CommunitiesRepository.createProxy(vertx).updateCommunity(community, testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not update a community if it is not defined.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunity(Community, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateACommunityThatCanNotBeAnObject(Vertx vertx, VertxTestContext testContext) {

		final Community community = new Community() {

			/**
			 * {@inheritDoc}
			 */
			@Override
			public JsonObject toJsonObject() {

				return null;
			}
		};
		community.id = "undefined community identifier";
		CommunitiesRepository.createProxy(vertx).updateCommunity(community, testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can update a community.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunity(Community, io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdateCommunity(Vertx vertx, VertxTestContext testContext) {

		final Community community = new Community();

		CommunitiesRepository.createProxy(vertx).storeCommunity(community,
				testContext.succeeding(stored -> testContext.verify(() -> {

					final Community update = new CommunityTest().createModelExample(23);
					update.id = stored.id;
					CommunitiesRepository.createProxy(vertx).updateCommunity(update,
							testContext.succeeding(empty -> testContext.verify(() -> {

								CommunitiesRepository.createProxy(vertx).searchCommunity(stored.id,
										testContext.succeeding(foundCommunity -> testContext.verify(() -> {
											assertThat(foundCommunity).isEqualTo(update);
											testContext.completeNow();
										})));
							})));

				})));

	}

	/**
	 * Verify that update a defined community object.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityObject(String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdateCommunityObject(Vertx vertx, VertxTestContext testContext) {

		CommunitiesRepository.createProxy(vertx).storeCommunity(new JsonObject().put("name", "Community name"),
				testContext.succeeding(stored -> testContext.verify(() -> {

					final String id = stored.getString("id");
					final JsonObject update = new JsonObject().put("id", id).put("name", "New community name");
					CommunitiesRepository.createProxy(vertx).updateCommunity(update,
							testContext.succeeding(empty -> testContext.verify(() -> {

								CommunitiesRepository.createProxy(vertx).searchCommunityObject(id,
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
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#deleteCommunity(String, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotDeleteUndefinedCommunity(Vertx vertx, VertxTestContext testContext) {

		CommunitiesRepository.createProxy(vertx).deleteCommunity("undefined community identifier",
				testContext.failing(failed -> {
					testContext.completeNow();
				}));

	}

	/**
	 * Verify that can delete a community.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#deleteCommunity(String, io.vertx.core.Handler)
	 */
	@Test
	public void shouldDeleteCommunity(Vertx vertx, VertxTestContext testContext) {

		CommunitiesRepository.createProxy(vertx).storeCommunity(new JsonObject(), testContext.succeeding(stored -> {

			final String id = stored.getString("id");
			CommunitiesRepository.createProxy(vertx).deleteCommunity(id, testContext.succeeding(success -> {

				CommunitiesRepository.createProxy(vertx).searchCommunityObject(id, testContext.failing(search -> {

					testContext.completeNow();

				}));

			}));

		}));

	}

	/**
	 * Remove all the communities defined on the
	 * CommunitiesRepository.createProxy(vertx).
	 *
	 * @param pool that create the mongo connections.
	 */
	public static final void removeAllCommunities(MongoClient pool) {

		final Semaphore semaphore = new Semaphore(0);
		pool.removeDocuments(CommunitiesRepositoryImpl.COMMUNITIES_COLLECTION, new JsonObject(), remove -> {

			semaphore.release();
		});

		try {
			semaphore.acquire();
		} catch (final InterruptedException ignored) {
		}

	}

	// /**
	// * Remove all the communities defined on the
	// * CommunitiesRepository.createProxy(vertx).
	// *
	// * @param repository to use.
	// * @param max number of communities to try to create.
	// *
	// * @return the communities that has been created.
	// */
	// public static final <T extends CommunitiesRepository> List<Community>
	// createAndStoreSomeCommunities(T repository,
	// int max) {
	//
	// final List<Community> communities = new ArrayList<>();
	// final Semaphore semaphore = new Semaphore(0);
	// createNextCommunity(repository, communities, max, semaphore);
	//
	// try {
	// semaphore.acquire(max);
	// } catch (final InterruptedException ignored) {
	// }
	//
	// return communities;
	// }

	// /**
	// * Create an store a community.
	// *
	// * @param repository to use.
	// * @param communities that has been created.
	// * @param tries number maximum of times to create a community.
	// * @param semaphore to inform when the community is created.
	// */
	// private static <T extends CommunitiesRepository> void createNextCommunity(T
	// repository, List<Community> communities,
	// int tries, Semaphore semaphore) {
	//
	// final Community community = new
	// CommunityTest().createModelExample(communities.size());
	// CommunitiesRepository.createProxy(vertx).storeCommunity(community, stored ->
	// {
	// if (!stored.failed()) {
	//
	// communities.add(stored.result());
	//
	// }
	// if (tries > 1) {
	// createNextCommunity(repository, communities, tries - 1, semaphore);
	// }
	// semaphore.release();
	// });
	//
	// }
	//
	// /**
	// * Verify that can find all the communities.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see CommunitiesRepository#searchCommunitiesPageObject(String, String,
	// List,
	// * String, Long, Long, int, int, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindAllCommunities(Vertx vertx, VertxTestContext
	// testContext) {
	//
	// removeAllCommunities(pool);
	// final List<Community> communities = createAndStoreSomeCommunities(repository,
	// 23);
	//
	// final String name = null;
	// final String description = null;
	// final List<String> keywords = null;
	// final String avatar = null;
	// final Long sinceFrom = null;
	// final Long sinceTo = null;
	// final int offset = 0;
	// final int limit = 100;
	// CommunitiesRepository.createProxy(vertx).searchCommunitiesPageObject(name,
	// description, keywords, avatar, sinceFrom,
	// sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(()
	// -> {
	//
	// final CommunitiesPage foundPage = Model.fromJsonObject(found,
	// CommunitiesPage.class);
	// assertThat(foundPage.offset).isEqualTo(offset);
	// assertThat(foundPage.total).isEqualTo(23);
	// assertThat(foundPage.communities).isEqualTo(communities);
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that can find some communities on a range.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see CommunitiesRepository#searchCommunitiesPageObject(String, String,
	// List,
	// * String, Long, Long, int, int, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindCommunitiesInARange(Vertx vertx, VertxTestContext
	// testContext) {
	//
	// removeAllCommunities(pool);
	// final List<Community> communities = createAndStoreSomeCommunities(repository,
	// 23);
	//
	// final String name = null;
	// final String description = null;
	// final List<String> keywords = null;
	// final String avatar = null;
	// final Long sinceFrom = null;
	// final Long sinceTo = null;
	// final int offset = 5;
	// final int limit = 10;
	// CommunitiesRepository.createProxy(vertx).searchCommunitiesPageObject(name,
	// description, keywords, avatar, sinceFrom,
	// sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(()
	// -> {
	//
	// final CommunitiesPage foundPage = Model.fromJsonObject(found,
	// CommunitiesPage.class);
	// assertThat(foundPage.offset).isEqualTo(offset);
	// assertThat(foundPage.total).isEqualTo(23);
	// assertThat(foundPage.communities).isEqualTo(communities.subList(5, 15));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that can find some communities with a specific name.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see CommunitiesRepository#searchCommunitiesPageObject(String, String,
	// List,
	// * String, Long, Long, int, int, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindCommunitiesWithAName(Vertx vertx, VertxTestContext
	// testContext) {
	//
	// removeAllCommunities(pool);
	// final List<Community> communities = createAndStoreSomeCommunities(repository,
	// 23);
	//
	// final String name = ".+1\\d";
	// final String description = null;
	// final List<String> keywords = null;
	// final String avatar = null;
	// final Long sinceFrom = null;
	// final Long sinceTo = null;
	// final int offset = 0;
	// final int limit = 100;
	// CommunitiesRepository.createProxy(vertx).searchCommunitiesPageObject(name,
	// description, keywords, avatar, sinceFrom,
	// sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(()
	// -> {
	//
	// final CommunitiesPage foundPage = Model.fromJsonObject(found,
	// CommunitiesPage.class);
	// assertThat(foundPage.offset).isEqualTo(0);
	// assertThat(foundPage.total).isEqualTo(10);
	// assertThat(foundPage.communities).isEqualTo(communities.subList(10, 20));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that can find some communities with a specific description.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see CommunitiesRepository#searchCommunitiesPageObject(String, String,
	// List,
	// * String, Long, Long, int, int, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindCommunitiesWithADescription(Vertx vertx,
	// VertxTestContext testContext) {
	//
	// removeAllCommunities(pool);
	// final List<Community> communities = createAndStoreSomeCommunities(repository,
	// 23);
	//
	// final String name = null;
	// final String description = ".+2\\d";
	// final List<String> keywords = null;
	// final String avatar = null;
	// final Long sinceFrom = null;
	// final Long sinceTo = null;
	// final int offset = 0;
	// final int limit = 100;
	// CommunitiesRepository.createProxy(vertx).searchCommunitiesPageObject(name,
	// description, keywords, avatar, sinceFrom,
	// sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(()
	// -> {
	//
	// final CommunitiesPage foundPage = Model.fromJsonObject(found,
	// CommunitiesPage.class);
	// assertThat(foundPage.offset).isEqualTo(0);
	// assertThat(foundPage.total).isEqualTo(3);
	// assertThat(foundPage.communities).isEqualTo(communities.subList(20, 23));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that can find some communities with a specific keywords.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see CommunitiesRepository#searchCommunitiesPageObject(String, String,
	// List,
	// * String, Long, Long, int, int, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindCommunitiesWithKeywords(Vertx vertx, VertxTestContext
	// testContext) {
	//
	// removeAllCommunities(pool);
	// final List<Community> communities = createAndStoreSomeCommunities(repository,
	// 23);
	//
	// final String name = null;
	// final String description = null;
	// final List<String> keywords = new ArrayList<>();
	// keywords.add("keyword 19");
	// keywords.add("keyword 21");
	// final String avatar = null;
	// final Long sinceFrom = null;
	// final Long sinceTo = null;
	// final int offset = 0;
	// final int limit = 100;
	// CommunitiesRepository.createProxy(vertx).searchCommunitiesPageObject(name,
	// description, keywords, avatar, sinceFrom,
	// sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(()
	// -> {
	//
	// final CommunitiesPage foundPage = Model.fromJsonObject(found,
	// CommunitiesPage.class);
	// assertThat(foundPage.offset).isEqualTo(0);
	// assertThat(foundPage.total).isEqualTo(2);
	// assertThat(foundPage.communities).isEqualTo(communities.subList(20, 22));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that can find some communities with a one keyword.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see CommunitiesRepository#searchCommunitiesPageObject(String, String,
	// List,
	// * String, Long, Long, int, int, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindCommunitiesWithKeyword(Vertx vertx, VertxTestContext
	// testContext) {
	//
	// removeAllCommunities(pool);
	// final List<Community> communities = createAndStoreSomeCommunities(repository,
	// 23);
	//
	// final String name = null;
	// final String description = null;
	// final List<String> keywords = new ArrayList<>();
	// keywords.add("keyword 19");
	// final String avatar = null;
	// final Long sinceFrom = null;
	// final Long sinceTo = null;
	// final int offset = 0;
	// final int limit = 100;
	// CommunitiesRepository.createProxy(vertx).searchCommunitiesPageObject(name,
	// description, keywords, avatar, sinceFrom,
	// sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(()
	// -> {
	//
	// final CommunitiesPage foundPage = Model.fromJsonObject(found,
	// CommunitiesPage.class);
	// assertThat(foundPage.offset).isEqualTo(0);
	// assertThat(foundPage.total).isEqualTo(4);
	// assertThat(foundPage.communities).isEqualTo(communities.subList(18, 22));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that can find some communities with a specific avatar.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see CommunitiesRepository#searchCommunitiesPageObject(String, String,
	// List,
	// * String, Long, Long, int, int, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindCommunitiesWithAnAvatar(Vertx vertx, VertxTestContext
	// testContext) {
	//
	// removeAllCommunities(pool);
	// final List<Community> communities = createAndStoreSomeCommunities(repository,
	// 23);
	//
	// final String name = null;
	// final String description = null;
	// final List<String> keywords = null;
	// final String avatar = ".+r\\d\\.png";
	// final Long sinceFrom = null;
	// final Long sinceTo = null;
	// final int offset = 0;
	// final int limit = 100;
	// CommunitiesRepository.createProxy(vertx).searchCommunitiesPageObject(name,
	// description, keywords, avatar, sinceFrom,
	// sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(()
	// -> {
	//
	// final CommunitiesPage foundPage = Model.fromJsonObject(found,
	// CommunitiesPage.class);
	// assertThat(foundPage.offset).isEqualTo(0);
	// assertThat(foundPage.total).isEqualTo(10);
	// assertThat(foundPage.communities).isEqualTo(communities.subList(0, 10));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Create an aggregate some communities with a fake
	// * {@link Community#_creationTs}.
	// *
	// * @param pool that create the mongo connections.
	// * @param max number of communities to try to create.
	// *
	// * @return the aggregated communities.
	// */
	// public static List<Community>
	// createAndStoreSomeCommunitiesWithFake_creationTs(int max) {
	//
	// final List<Community> communities = new ArrayList<>();
	// final Semaphore semaphore = new Semaphore(0);
	// createNextCommunityWithFake_creationTs(pool, communities, max, semaphore);
	//
	// try {
	// semaphore.acquire(max);
	// } catch (final InterruptedException ignored) {
	// }
	//
	// return communities;
	//
	// }
	//
	// /**
	// * Create an store a community with a fake since time.
	// *
	// * @param pool that create the mongo connections.
	// * @param communities that has been created.
	// * @param tries number maximum of times to create a community.
	// * @param semaphore to inform when the community is created.
	// */
	// private static void createNextCommunityWithFake_creationTs(List<Community>
	// communities, int tries,
	// Semaphore semaphore) {
	//
	// final int index = communities.size();
	// final Community community = new CommunityTest().createModelExample(index);
	// community._creationTs = index * 100000;
	// pool.save(CommunitiesRepositoryImpl.COMMUNITIES_COLLECTION,
	// community.toJsonObject(), stored -> {
	// if (!stored.failed()) {
	//
	// community.id = stored.result();
	// communities.add(community);
	//
	// }
	// if (tries > 1) {
	// createNextCommunityWithFake_creationTs(pool, communities, tries - 1,
	// semaphore);
	// }
	// semaphore.release();
	// });
	//
	// }
	//
	// /**
	// * Verify that can find some communities since a from time.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see CommunitiesRepository#searchCommunitiesPageObject(String, String,
	// List,
	// * String, Long, Long, int, int, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindCommunitiesSinceAFromTime(Vertx vertx, VertxTestContext
	// testContext) {
	//
	// removeAllCommunities(pool);
	// final List<Community> communities =
	// createAndStoreSomeCommunitiesWithFake_creationTs(pool, 23);
	//
	// final String name = null;
	// final String description = null;
	// final List<String> keywords = null;
	// final String avatar = null;
	// final Long sinceFrom = 1500000l;
	// final Long sinceTo = null;
	// final int offset = 0;
	// final int limit = 100;
	// CommunitiesRepository.createProxy(vertx).searchCommunitiesPageObject(name,
	// description, keywords, avatar, sinceFrom,
	// sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(()
	// -> {
	//
	// final CommunitiesPage foundPage = Model.fromJsonObject(found,
	// CommunitiesPage.class);
	// assertThat(foundPage.offset).isEqualTo(0);
	// assertThat(foundPage.total).isEqualTo(8);
	// assertThat(foundPage.communities).isEqualTo(communities.subList(15, 23));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that can find some communities since a to time.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see CommunitiesRepository#searchCommunitiesPageObject(String, String,
	// List,
	// * String, Long, Long, int, int, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindCommunitiesSinceAToTime(Vertx vertx, VertxTestContext
	// testContext) {
	//
	// removeAllCommunities(pool);
	// final List<Community> communities =
	// createAndStoreSomeCommunitiesWithFake_creationTs(pool, 23);
	//
	// final String name = null;
	// final String description = null;
	// final List<String> keywords = null;
	// final String avatar = null;
	// final Long sinceFrom = null;
	// final Long sinceTo = 1500000l;
	// final int offset = 0;
	// final int limit = 100;
	// CommunitiesRepository.createProxy(vertx).searchCommunitiesPageObject(name,
	// description, keywords, avatar, sinceFrom,
	// sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(()
	// -> {
	//
	// final CommunitiesPage foundPage = Model.fromJsonObject(found,
	// CommunitiesPage.class);
	// assertThat(foundPage.offset).isEqualTo(0);
	// assertThat(foundPage.total).isEqualTo(16);
	// assertThat(foundPage.communities).isEqualTo(communities.subList(0, 16));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that can find some communities since time in a range.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see CommunitiesRepository#searchCommunitiesPageObject(String, String,
	// List,
	// * String, Long, Long, int, int, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindCommunities_creationTsInARange(Vertx vertx,
	// VertxTestContext testContext) {
	//
	// removeAllCommunities(pool);
	// final List<Community> communities =
	// createAndStoreSomeCommunitiesWithFake_creationTs(pool, 23);
	//
	// final String name = null;
	// final String description = null;
	// final List<String> keywords = null;
	// final String avatar = null;
	// final Long sinceFrom = 1200000l;
	// final Long sinceTo = 1800000l;
	// final int offset = 0;
	// final int limit = 100;
	// CommunitiesRepository.createProxy(vertx).searchCommunitiesPageObject(name,
	// description, keywords, avatar, sinceFrom,
	// sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(()
	// -> {
	//
	// final CommunitiesPage foundPage = Model.fromJsonObject(found,
	// CommunitiesPage.class);
	// assertThat(foundPage.offset).isEqualTo(0);
	// assertThat(foundPage.total).isEqualTo(7);
	// assertThat(foundPage.communities).isEqualTo(communities.subList(12, 19));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that can find some communities.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see CommunitiesRepository#searchCommunitiesPageObject(String, String,
	// List,
	// * String, Long, Long, int, int, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindCommunities(Vertx vertx, VertxTestContext testContext)
	// {
	//
	// removeAllCommunities(pool);
	// final List<Community> communities =
	// createAndStoreSomeCommunitiesWithFake_creationTs(pool, 23);
	//
	// final String name = ".+ 1\\d";
	// final String description = "3|4|5|6";
	// final List<String> keywords = new ArrayList<>();
	// keywords.add("\\d{2}");
	// final String avatar = ".*png";
	// final Long sinceFrom = 1200000l;
	// final Long sinceTo = 1800000l;
	// final int offset = 1;
	// final int limit = 2;
	// CommunitiesRepository.createProxy(vertx).searchCommunitiesPageObject(name,
	// description, keywords, avatar, sinceFrom,
	// sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(()
	// -> {
	//
	// final CommunitiesPage foundPage = Model.fromJsonObject(found,
	// CommunitiesPage.class);
	// assertThat(foundPage.offset).isEqualTo(1);
	// assertThat(foundPage.total).isEqualTo(4);
	// assertThat(foundPage.communities).isEqualTo(communities.subList(14, 16));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that can find an empty community page if any community match.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see CommunitiesRepository#searchCommunitiesPageObject(String, String,
	// List,
	// * String, Long, Long, int, int, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindEmptyCommunitiesPageIfAnyMatch(Vertx vertx,
	// VertxTestContext testContext) {
	//
	// removeAllCommunities(pool);
	// final String name = null;
	// final String description = null;
	// final List<String> keywords = new ArrayList<>();
	// final String avatar = null;
	// final Long sinceFrom = null;
	// final Long sinceTo = null;
	// final int offset = 0;
	// final int limit = 100;
	// CommunitiesRepository.createProxy(vertx).searchCommunitiesPageObject(name,
	// description, keywords, avatar, sinceFrom,
	// sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(()
	// -> {
	//
	// final CommunitiesPage foundPage = Model.fromJsonObject(found,
	// CommunitiesPage.class);
	// assertThat(foundPage.offset).isEqualTo(0);
	// assertThat(foundPage.total).isEqualTo(0);
	// assertThat(foundPage.communities).isNull();
	//
	// testContext.completeNow();
	// })));
	//
	// }

	// /**
	// * Verify that can find an empty community page if any community match.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see CommunitiesRepository#searchCommunitiesPageObject(String, String,
	// List,
	// * String, Long, Long, int, int, io.vertx.core.Handler)
	// */
	// @Test
	// @Execution(ExecutionMode.SAME_THREAD)
	// public void shouldFindEmptyCommunitiesPageIfOffsetIsOutOfRange(Vertx vertx,
	// VertxTestContext testContext) {
	//
	// removeAllCommunities(pool);
	// createAndStoreSomeCommunities(repository, 2);
	// final String name = null;
	// final String description = null;
	// final List<String> keywords = new ArrayList<>();
	// final String avatar = null;
	// final Long sinceFrom = null;
	// final Long sinceTo = null;
	// final int offset = 3;
	// final int limit = 100;
	// CommunitiesRepository.createProxy(vertx).searchCommunitiesPageObject(name,
	// description, keywords, avatar, sinceFrom,
	// sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(()
	// -> {
	//
	// final CommunitiesPage foundPage = Model.fromJsonObject(found,
	// CommunitiesPage.class);
	// assertThat(foundPage.offset).isEqualTo(3);
	// assertThat(foundPage.total).isEqualTo(2);
	// assertThat(foundPage.communities).isNull();
	//
	// testContext.completeNow();
	// })));
	//
	// }

	/**
	 * Verify that can not found a community member.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMember(String, String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotFoundCommunityMember(Vertx vertx, VertxTestContext testContext) {

		CommunitiesRepository.createProxy(vertx).searchCommunityMember("undefined community identifier",
				"undefined user id", testContext.failing(failed -> {
					testContext.completeNow();
				}));

	}

	/**
	 * Verify that can found a community member.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMember(String, String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityMember(Vertx vertx, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final CommunityMember member = new CommunityMember();
		member.userId = UUID.randomUUID().toString();
		final long now = TimeManager.now();
		CommunitiesRepository.createProxy(vertx).storeCommunityMember(communityId, member,
				testContext.succeeding(storedMember -> {
					CommunitiesRepository.createProxy(vertx).searchCommunityMember(communityId, member.userId,
							testContext.succeeding(foundCommunity -> testContext.verify(() -> {
								final CommunityMember expectedMember = new CommunityMember();
								expectedMember.userId = member.userId;
								expectedMember.joinTime = foundCommunity.joinTime;
								assertThat(foundCommunity).isEqualTo(expectedMember);
								assertThat(foundCommunity.joinTime).isGreaterThanOrEqualTo(now);
								testContext.completeNow();
							})));
				}));

	}

	/**
	 * Verify that can found a community member object.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMemberObject(String, String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityMemberObject(Vertx vertx, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final String userId = UUID.randomUUID().toString();
		final long now = TimeManager.now();
		CommunitiesRepository.createProxy(vertx).storeCommunityMemberObject(communityId,
				new JsonObject().put("userId", userId), testContext.succeeding(storedMember -> {
					CommunitiesRepository.createProxy(vertx).searchCommunityMemberObject(communityId, userId,
							testContext.succeeding(foundCommunity -> testContext.verify(() -> {
								assertThat(foundCommunity).isNotNull();
								assertThat(foundCommunity.getString("userId")).isEqualTo(userId);
								assertThat(foundCommunity.getLong("joinTime")).isGreaterThanOrEqualTo(now);
								testContext.completeNow();
							})));
				}));

	}

	/**
	 * Verify that can not store a community member that can not be an object.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunityMember(String,CommunityMember,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotStoreACommunityMemberThatCanNotBeAnObject(Vertx vertx, VertxTestContext testContext) {

		final CommunityMember communityMember = new CommunityMember() {

			/**
			 * {@inheritDoc}
			 */
			@Override
			public JsonObject toJsonObject() {

				return null;
			}
		};
		CommunitiesRepository.createProxy(vertx).storeCommunityMember("communityId", communityMember,
				testContext.failing(failed -> {
					testContext.completeNow();
				}));

	}

	/**
	 * Verify that can store a community member.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunityMember(String, CommunityMember,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldStoreCommunityMember(Vertx vertx, VertxTestContext testContext) {

		final long now = TimeManager.now();
		final CommunityMember communityMember = new CommunityMember();
		CommunitiesRepository.createProxy(vertx).storeCommunityMember("communityId", communityMember,
				testContext.succeeding(storedCommunityMember -> testContext.verify(() -> {

					assertThat(storedCommunityMember).isNotNull();
					assertThat(storedCommunityMember.joinTime).isGreaterThanOrEqualTo(now);
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can store a community member object.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunityMemberObject(String,JsonObject,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldStoreCommunityMemberObject(Vertx vertx, VertxTestContext testContext) {

		final long now = TimeManager.now();
		CommunitiesRepository.createProxy(vertx).storeCommunityMemberObject("communityId", new JsonObject(),
				testContext.succeeding(storedCommunityMember -> testContext.verify(() -> {

					assertThat(storedCommunityMember).isNotNull();
					assertThat(storedCommunityMember.getLong("joinTime", 0l)).isNotEqualTo(0).isGreaterThanOrEqualTo(now);
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can not update a community member if it is not defined.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunityMember(String,CommunityMember,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateUndefinedCommunityMember(Vertx vertx, VertxTestContext testContext) {

		final CommunityMember communityMember = new CommunityMember();
		communityMember.userId = "undefined community member identifier";
		CommunitiesRepository.createProxy(vertx).updateCommunityMember("communityId", communityMember,
				testContext.failing(failed -> {
					testContext.completeNow();
				}));

	}

	/**
	 * Verify that can not update a community member if it is not defined.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunityMemberObject(String,JsonObject,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateUndefinedCommunityMemberObject(Vertx vertx, VertxTestContext testContext) {

		final JsonObject communityMember = new JsonObject().put("userId", "undefined community member identifier");
		CommunitiesRepository.createProxy(vertx).updateCommunityMemberObject("communityId", communityMember,
				testContext.failing(failed -> {
					testContext.completeNow();
				}));

	}

	/**
	 * Verify that can not update a community member if it is not defined.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunityMember(String,CommunityMember,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateACommunityMemberThatCanNotBeAnObject(Vertx vertx, VertxTestContext testContext) {

		final CommunityMember communityMember = new CommunityMember() {

			/**
			 * {@inheritDoc}
			 */
			@Override
			public JsonObject toJsonObject() {

				return null;
			}
		};
		communityMember.userId = "undefined community member identifier";
		CommunitiesRepository.createProxy(vertx).updateCommunityMember("communityId", communityMember,
				testContext.failing(failed -> {
					testContext.completeNow();
				}));

	}

	/**
	 * Verify that update a community member.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunityMember(String,CommunityMember,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdateACommunityMember(Vertx vertx, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final CommunityMember communityMember = new CommunityMember();
		communityMember.userId = UUID.randomUUID().toString();
		CommunitiesRepository.createProxy(vertx).storeCommunityMember(communityId, communityMember,
				testContext.succeeding(stored -> {

					final CommunityMember member = new CommunityMember();
					member.userId = communityMember.userId;
					CommunitiesRepository.createProxy(vertx).updateCommunityMember(communityId, member,
							testContext.succeeding(update -> {
								testContext.completeNow();
							}));

				}));

	}

	/**
	 * Verify that delete a community member.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#deleteCommunityMember(String, String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldDeleteCommunityMember(Vertx vertx, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final CommunityMember communityMember = new CommunityMember();
		final String userId = UUID.randomUUID().toString();
		communityMember.userId = userId;
		CommunitiesRepository.createProxy(vertx).storeCommunityMember(communityId, communityMember,
				testContext.succeeding(stored -> {

					CommunitiesRepository.createProxy(vertx).deleteCommunityMember(communityId, userId,
							testContext.succeeding(delete -> {

								CommunitiesRepository.createProxy(vertx).searchCommunityMember(communityId, userId,
										testContext.failing(search -> {
											testContext.completeNow();
										}));

							}));
				}));

	}

	/**
	 * Verify that found an empty community member page.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMembersPageObject(String, Long,
	 *      Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundEmptyCommunityMemberPage(Vertx vertx, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final CommunityMember communityMember = new CommunityMember();
		final String userId = UUID.randomUUID().toString();
		communityMember.userId = userId;
		CommunitiesRepository.createProxy(vertx).searchCommunityMembersPageObject(communityId, null, null, 0, 100,
				testContext.succeeding(page -> testContext.verify(() -> {

					assertThat(page.getLong("offset")).isEqualTo(0L);
					assertThat(page.getLong("total")).isEqualTo(0L);
					assertThat(page.getJsonArray("members")).isNull();
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that found some community user members.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMembersPageObject(String, Long,
	 *      Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityMemberPage(Vertx vertx, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		CommunitiesRepository.createProxy(vertx).storeCommunityMemberObject(communityId,
				new JsonObject().put("userId", UUID.randomUUID().toString()), testContext.succeeding(stored1 -> {

					CommunitiesRepository.createProxy(vertx).storeCommunityMemberObject(communityId,
							new JsonObject().put("userId", UUID.randomUUID().toString()), testContext.succeeding(stored2 -> {

								CommunitiesRepository.createProxy(vertx).searchCommunityMembersPageObject(communityId, null, null, 0,
										100, testContext.succeeding(page -> testContext.verify(() -> {

											assertThat(page.getLong("offset")).isEqualTo(0L);
											assertThat(page.getLong("total")).isEqualTo(2L);
											final JsonArray members = page.getJsonArray("members");
											assertThat(members.size()).isEqualTo(2);
											assertThat(members.getJsonObject(0).getString("userId")).isEqualTo(stored1.getString("userId"));
											assertThat(members.getJsonObject(1).getString("userId")).isEqualTo(stored2.getString("userId"));
											testContext.completeNow();
										})));

							}));
				}));

	}

	/**
	 * Verify that found some community user members with a join from.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMembersPageObject(String, Long,
	 *      Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityMemberPageWithJoinFrom(Vertx vertx, VertxTestContext testContext) {

		final long now = TimeManager.now();
		final String communityId = UUID.randomUUID().toString();
		CommunitiesRepository.createProxy(vertx).storeCommunityMemberObject(communityId,
				new JsonObject().put("userId", UUID.randomUUID().toString()), testContext.succeeding(stored1 -> {

					CommunitiesRepository.createProxy(vertx).storeCommunityMemberObject(communityId,
							new JsonObject().put("userId", UUID.randomUUID().toString()), testContext.succeeding(stored2 -> {

								CommunitiesRepository.createProxy(vertx).searchCommunityMembersPageObject(communityId, now, null, 0,
										100, testContext.succeeding(page -> testContext.verify(() -> {

											assertThat(page.getLong("offset")).isEqualTo(0L);
											assertThat(page.getLong("total")).isEqualTo(2L);
											final JsonArray members = page.getJsonArray("members");
											assertThat(members.size()).isEqualTo(2);
											assertThat(members.getJsonObject(0).getString("userId")).isEqualTo(stored1.getString("userId"));
											assertThat(members.getJsonObject(1).getString("userId")).isEqualTo(stored2.getString("userId"));
											testContext.completeNow();
										})));

							}));
				}));

	}

	/**
	 * Verify that found some community user members with a join to.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMembersPageObject(String, Long,
	 *      Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityMemberPageWithJoinTo(Vertx vertx, VertxTestContext testContext) {

		final long now = TimeManager.now();
		final String communityId = UUID.randomUUID().toString();
		CommunitiesRepository.createProxy(vertx).storeCommunityMemberObject(communityId,
				new JsonObject().put("userId", UUID.randomUUID().toString()), testContext.succeeding(stored1 -> {

					CommunitiesRepository.createProxy(vertx).storeCommunityMemberObject(communityId,
							new JsonObject().put("userId", UUID.randomUUID().toString()), testContext.succeeding(stored2 -> {

								CommunitiesRepository.createProxy(vertx).searchCommunityMembersPageObject(communityId, null, now + 1, 0,
										100, testContext.succeeding(page -> testContext.verify(() -> {

											assertThat(page.getLong("offset")).isEqualTo(0L);
											assertThat(page.getLong("total")).isEqualTo(2L);
											final JsonArray members = page.getJsonArray("members");
											assertThat(members.size()).isEqualTo(2);
											assertThat(members.getJsonObject(0).getString("userId")).isEqualTo(stored1.getString("userId"));
											assertThat(members.getJsonObject(1).getString("userId")).isEqualTo(stored2.getString("userId"));
											testContext.completeNow();
										})));

							}));
				}));

	}

	/**
	 * Verify that can not found a community norm.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityNorm(String, String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotFoundCommunityNorm(Vertx vertx, VertxTestContext testContext) {

		CommunitiesRepository.createProxy(vertx).searchCommunityNorm("undefined community identifier", "undefined norm id",
				testContext.failing(failed -> {
					testContext.completeNow();
				}));

	}

	/**
	 * Verify that can found a community norm.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityNorm(String, String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityNorm(Vertx vertx, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final CommunityNorm norm = new CommunityNorm();
		norm.id = UUID.randomUUID().toString();
		final long now = TimeManager.now();
		CommunitiesRepository.createProxy(vertx).storeCommunityNorm(communityId, norm,
				testContext.succeeding(storedNorm -> {
					CommunitiesRepository.createProxy(vertx).searchCommunityNorm(communityId, norm.id,
							testContext.succeeding(foundCommunity -> testContext.verify(() -> {
								final CommunityNorm expectedNorm = new CommunityNorm();
								expectedNorm.id = norm.id;
								expectedNorm._creationTs = foundCommunity._creationTs;
								assertThat(foundCommunity).isEqualTo(expectedNorm);
								assertThat(foundCommunity._creationTs).isGreaterThanOrEqualTo(now);
								testContext.completeNow();
							})));
				}));

	}

	/**
	 * Verify that can found a community norm object.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityNormObject(String, String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityNormObject(Vertx vertx, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final long now = TimeManager.now();
		CommunitiesRepository.createProxy(vertx).storeCommunityNormObject(communityId, new JsonObject(),
				testContext.succeeding(storedNorm -> {
					final String normId = storedNorm.getString("id");
					CommunitiesRepository.createProxy(vertx).searchCommunityNormObject(communityId, normId,
							testContext.succeeding(foundCommunity -> testContext.verify(() -> {
								assertThat(foundCommunity).isNotNull();
								assertThat(foundCommunity.getString("id")).isEqualTo(normId);
								assertThat(foundCommunity.getLong("_creationTs")).isGreaterThanOrEqualTo(now);
								testContext.completeNow();
							})));
				}));

	}

	/**
	 * Verify that can not store a community norm that can not be an object.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunityNorm(String,CommunityNorm,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotStoreACommunityNormThatCanNotBeAnObject(Vertx vertx, VertxTestContext testContext) {

		final CommunityNorm communityNorm = new CommunityNorm() {

			/**
			 * {@inheritDoc}
			 */
			@Override
			public JsonObject toJsonObject() {

				return null;
			}
		};
		CommunitiesRepository.createProxy(vertx).storeCommunityNorm("communityId", communityNorm,
				testContext.failing(failed -> {
					testContext.completeNow();
				}));

	}

	/**
	 * Verify that can store a community norm.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunityNorm(String, CommunityNorm,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldStoreCommunityNorm(Vertx vertx, VertxTestContext testContext) {

		final long now = TimeManager.now();
		final CommunityNorm communityNorm = new CommunityNorm();
		CommunitiesRepository.createProxy(vertx).storeCommunityNorm("communityId", communityNorm,
				testContext.succeeding(storedCommunityNorm -> testContext.verify(() -> {

					assertThat(storedCommunityNorm).isNotNull();
					assertThat(storedCommunityNorm._creationTs).isGreaterThanOrEqualTo(now);
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can store a community norm object.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunityNormObject(String,JsonObject,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldStoreCommunityNormObject(Vertx vertx, VertxTestContext testContext) {

		final long now = TimeManager.now();
		CommunitiesRepository.createProxy(vertx).storeCommunityNormObject("communityId", new JsonObject(),
				testContext.succeeding(storedCommunityNorm -> testContext.verify(() -> {

					assertThat(storedCommunityNorm).isNotNull();
					assertThat(storedCommunityNorm.getLong("_creationTs", 0l)).isNotEqualTo(0).isGreaterThanOrEqualTo(now);
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that delete a community norm.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#deleteCommunityNorm(String, String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldDeleteCommunityNorm(Vertx vertx, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final CommunityNorm communityNorm = new CommunityNorm();
		final String normId = UUID.randomUUID().toString();
		communityNorm.id = normId;
		CommunitiesRepository.createProxy(vertx).storeCommunityNorm(communityId, communityNorm,
				testContext.succeeding(stored -> {

					CommunitiesRepository.createProxy(vertx).deleteCommunityNorm(communityId, normId,
							testContext.succeeding(delete -> {

								CommunitiesRepository.createProxy(vertx).searchCommunityNorm(communityId, normId,
										testContext.failing(search -> {
											testContext.completeNow();
										}));

							}));
				}));

	}

	/**
	 * Verify that found an empty community norm page.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityNormsPageObject(String, Long, Long,
	 *      int, int, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundEmptyCommunityNormPage(Vertx vertx, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final CommunityNorm communityNorm = new CommunityNorm();
		final String normId = UUID.randomUUID().toString();
		communityNorm.id = normId;
		CommunitiesRepository.createProxy(vertx).searchCommunityNormsPageObject(communityId, null, null, 0, 100,
				testContext.succeeding(page -> testContext.verify(() -> {

					assertThat(page.getLong("offset")).isEqualTo(0L);
					assertThat(page.getLong("total")).isEqualTo(0L);
					assertThat(page.getJsonArray("norms")).isNull();
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that found some community norm norms.
	 *
	 * @param vertx       event bus to use.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityNormsPageObject(String, Long, Long,
	 *      int, int, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityNormPage(Vertx vertx, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		CommunitiesRepository.createProxy(vertx).storeCommunityNormObject(communityId, new JsonObject(),
				testContext.succeeding(stored1 -> {

					CommunitiesRepository.createProxy(vertx).storeCommunityNormObject(communityId, new JsonObject(),
							testContext.succeeding(stored2 -> {

								CommunitiesRepository.createProxy(vertx).searchCommunityNormsPageObject(communityId, null, null, 0, 100,
										testContext.succeeding(page -> testContext.verify(() -> {

											assertThat(page.getLong("offset")).isEqualTo(0L);
											assertThat(page.getLong("total")).isEqualTo(2L);
											final JsonArray norms = page.getJsonArray("norms", new JsonArray());
											assertThat(norms.size()).isEqualTo(2);
											assertThat(norms.getJsonObject(0).getString("id")).isEqualTo(stored1.getString("id"));
											assertThat(norms.getJsonObject(1).getString("id")).isEqualTo(stored2.getString("id"));
											testContext.completeNow();
										})));

							}));
				}));

	}

	// /**
	// * Create some community members
	// *
	// * @param communityId identifier of the community to add the members.
	// * @param pool connection to the mongo database.
	// * @param max number of members to create.
	// *
	// * @return the community members with a fake join time.
	// */
	// public static List<CommunityMember>
	// createCommunityMembersWithFakeJoinTime(String communityId,
	// int max) {
	//
	// final List<CommunityMember> members = new ArrayList<>();
	// final Semaphore semaphore = new Semaphore(0);
	//
	// createNextMember(semaphore, members, communityId, pool, max);
	//
	// try {
	// semaphore.acquire(max);
	// } catch (final InterruptedException ignored) {
	// }
	//
	// return members;
	//
	// }
	//
	// /**
	// * Create and store the next community member.
	// *
	// * @param semaphore to inform when the member is created.
	// * @param members list to store the created member.
	// * @param communityId identifier of the community to add the members.
	// * @param pool connection to the mongo database.
	// * @param max number of members to create.
	// */
	// private static void createNextMember(Semaphore semaphore,
	// List<CommunityMember> members, String communityId,
	// int max) {
	//
	// final String userId = UUID.randomUUID().toString();
	// final long joinTime = members.size() * 100000;
	// pool.save(CommunitiesRepositoryImpl.COMMUNITY_MEMBERS_COLLECTION,
	// new JsonObject().put("communityId", communityId).put("userId",
	// userId).put("joinTime", joinTime), save -> {
	//
	// if (save.failed()) {
	//
	// InternalLogger.log(Level.ERROR, save.cause());
	// }
	// final CommunityMember member = new CommunityMember();
	// member.userId = userId;
	// member.joinTime = joinTime;
	// members.add(member);
	// if (members.size() < max) {
	//
	// createNextMember(semaphore, members, communityId, pool, max);
	// }
	// semaphore.release();
	// });
	//
	// }
	//
	// /**
	// * Create an aggregate some communities with a fake
	// * {@link Community#_creationTs}.
	// *
	// * @param communityId identifier where the norm will be stored.
	// * @param pool that create the mongo connections.
	// * @param max number of communities to try to create.
	// *
	// * @return the aggregated communities.
	// */
	// public static List<CommunityNorm>
	// createAndStoreSomeCommunityNormsWithFake_creationTs(String communityId,
	// int max) {
	//
	// final List<CommunityNorm> communityNormss = new ArrayList<>();
	// final Semaphore semaphore = new Semaphore(0);
	// createNextCommunityNormWithFake_creationTs(communityId, pool,
	// communityNormss, max, semaphore);
	//
	// try {
	// semaphore.acquire(max);
	// } catch (final InterruptedException ignored) {
	// }
	//
	// return communityNormss;
	//
	// }
	//
	// /**
	// * Create an store a community norm with a fake since time.
	// *
	// * @param communityId identifier where the norm will be stored.
	// * @param pool that create the mongo connections.
	// * @param communityNorms that has been created.
	// * @param tries number maximum of times to create a community norm.
	// * @param semaphore to inform when the community is created.
	// */
	// private static void createNextCommunityNormWithFake_creationTs(String
	// communityId,
	// List<CommunityNorm> communityNorms, int tries, Semaphore semaphore) {
	//
	// final int index = communityNorms.size();
	// final CommunityNorm communityNorm = new
	// CommunityNormTest().createModelExample(index);
	// communityNorm._creationTs = index * 100000;
	// pool.save(CommunitiesRepositoryImpl.COMMUNITY_NORMS_COLLECTION,
	// communityNorm.toJsonObject().put("communityId", communityId), stored -> {
	// if (!stored.failed()) {
	//
	// communityNorm.id = stored.result();
	// communityNorms.add(communityNorm);
	//
	// }
	// if (tries > 1) {
	// createNextCommunityNormWithFake_creationTs(communityId, pool, communityNorms,
	// tries - 1, semaphore);
	// }
	// semaphore.release();
	// });
	//
	// }
	//
	// /**
	// * Verify that found some community norms with a since from.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see CommunitiesRepository#searchCommunityNormsPageObject(String, Long,
	// Long,
	// * int, int, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldFoundCommunityNormsPageWithSinceFrom( Vertx vertx,
	// VertxTestContext testContext) {
	//
	// final String communityId = UUID.randomUUID().toString();
	// final List<CommunityNorm> communityNorms =
	// createAndStoreSomeCommunityNormsWithFake_creationTs(communityId, pool,
	// 23);
	// CommunitiesRepository.createProxy(vertx).searchCommunityNormsPageObject(communityId,
	// 1500000l, null, 0, 100,
	// testContext.succeeding(found -> testContext.verify(() -> {
	//
	// final CommunityNormsPage foundPage = Model.fromJsonObject(found,
	// CommunityNormsPage.class);
	// assertThat(foundPage.offset).isEqualTo(0);
	// assertThat(foundPage.total).isEqualTo(8);
	// assertThat(foundPage.norms).isEqualTo(communityNorms.subList(15, 23));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that found some community norms with a since to.
	// *
	// * @param pool that create the mongo connections.
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see CommunitiesRepository#searchCommunityNormsPageObject(String, Long,
	// Long,
	// * int, int, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldFoundCommunityNormsPageWithSinceTo( Vertx vertx,
	// VertxTestContext testContext) {
	//
	// final String communityId = UUID.randomUUID().toString();
	// final List<CommunityNorm> communityNorms =
	// createAndStoreSomeCommunityNormsWithFake_creationTs(communityId, pool,
	// 23);
	// CommunitiesRepository.createProxy(vertx).searchCommunityNormsPageObject(communityId,
	// null, 1500000l, 0, 100,
	// testContext.succeeding(found -> testContext.verify(() -> {
	//
	// final CommunityNormsPage foundPage = Model.fromJsonObject(found,
	// CommunityNormsPage.class);
	// assertThat(foundPage.offset).isEqualTo(0);
	// assertThat(foundPage.total).isEqualTo(16);
	// assertThat(foundPage.norms).isEqualTo(communityNorms.subList(0, 16));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Verify that found some community norms with a since range.
	// *
	// * @param vertx event bus to use.
	// * @param testContext context that executes the test.
	// *
	// * @see CommunitiesRepository#searchCommunityNormsPageObject(String, Long,
	// Long,
	// * int, int, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldFoundCommunityNormsPageWithSinceRange( Vertx vertx,
	// VertxTestContext testContext) {
	//
	// final String communityId = UUID.randomUUID().toString();
	// final List<CommunityNorm> communityNorms =
	// createAndStoreSomeCommunityNormsWithFake_creationTs(communityId, pool,
	// 23);
	// CommunitiesRepository.createProxy(vertx).searchCommunityNormsPageObject(communityId,
	// 1500000l, 2100000l, 2, 3,
	// testContext.succeeding(found -> testContext.verify(() -> {
	//
	// final CommunityNormsPage foundPage = Model.fromJsonObject(found,
	// CommunityNormsPage.class);
	// assertThat(foundPage.offset).isEqualTo(2);
	// assertThat(foundPage.total).isEqualTo(7);
	// assertThat(foundPage.norms).isEqualTo(communityNorms.subList(17, 20));
	//
	// testContext.completeNow();
	// })));
	//
	// }
	//
	// /**
	// * Create some community norms
	// *
	// * @param communityId identifier of the community to add the norms.
	// * @param pool connection to the mongo database.
	// * @param max number of norms to create.
	// *
	// * @return the community norms with a fake since time.
	// */
	// public static List<CommunityNorm>
	// createCommunityNormsWithFake_creationTs(String communityId,
	// int max) {
	//
	// final List<CommunityNorm> norms = new ArrayList<>();
	// final Semaphore semaphore = new Semaphore(0);
	//
	// createNextNorm(semaphore, norms, communityId, pool, max);
	//
	// try {
	// semaphore.acquire(max);
	// } catch (final InterruptedException ignored) {
	// }
	//
	// return norms;
	//
	// }
	//
	// /**
	// * Create and store the next community norm.
	// *
	// * @param semaphore to inform when the norm is created.
	// * @param norms list to store the created norm.
	// * @param communityId identifier of the community to add the norms.
	// * @param pool connection to the mongo database.
	// * @param max number of norms to create.
	// */
	// private static void createNextNorm(Semaphore semaphore, List<CommunityNorm>
	// norms, String communityId,
	// int max) {
	//
	// final String userId = UUID.randomUUID().toString();
	// final long _creationTs = norms.size() * 100000;
	// pool.save(CommunitiesRepositoryImpl.COMMUNITY_NORMS_COLLECTION,
	// new JsonObject().put("communityId", communityId).put("id",
	// userId).put("_creationTs", _creationTs), save -> {
	//
	// if (save.failed()) {
	//
	// InternalLogger.log(Level.ERROR, save.cause());
	// }
	// final CommunityNorm norm = new CommunityNorm();
	// norm.id = userId;
	// norm._creationTs = _creationTs;
	// norms.add(norm);
	// if (norms.size() < max) {
	//
	// createNextNorm(semaphore, norms, communityId, pool, max);
	// }
	// semaphore.release();
	// });
	//
	// }

}
