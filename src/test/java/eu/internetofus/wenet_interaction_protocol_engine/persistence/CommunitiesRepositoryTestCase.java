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
import java.util.UUID;
import java.util.concurrent.Semaphore;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

import eu.internetofus.wenet_interaction_protocol_engine.Model;
import eu.internetofus.wenet_interaction_protocol_engine.TimeManager;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.CommunitiesPage;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.Community;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.CommunityMember;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.CommunityTest;
import eu.internetofus.wenet_interaction_protocol_engine.services.WeNetProfileManagerService;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
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
	 * @see CommunitiesRepository#storeCommunity(Community, io.vertx.core.Handler)
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
	 * @see CommunitiesRepository#storeCommunity(Community, io.vertx.core.Handler)
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

		final long now = TimeManager.now();
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
	 * @see CommunitiesRepository#searchCommunityObject(String,
	 *      io.vertx.core.Handler)
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
	 * @see CommunitiesRepository#deleteCommunity(String, io.vertx.core.Handler)
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
	 * @see CommunitiesRepository#deleteCommunity(String, io.vertx.core.Handler)
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

	/**
	 * Remove all the communities defined on the repository.
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

	/**
	 * Remove all the communities defined on the repository.
	 *
	 * @param repository to use.
	 * @param max        number of communities to try to create.
	 *
	 * @return the communities that has been created.
	 */
	public static final <T extends CommunitiesRepository> List<Community> createAndStoreSomeCommunities(T repository,
			int max) {

		final List<Community> communities = new ArrayList<>();
		final Semaphore semaphore = new Semaphore(0);
		createNextCommunity(repository, communities, max, semaphore);

		try {
			semaphore.acquire(max);
		} catch (final InterruptedException ignored) {
		}

		return communities;
	}

	/**
	 * Create an store a community.
	 *
	 * @param repository  to use.
	 * @param communities that has been created.
	 * @param tries       number maximum of times to create a community.
	 * @param semaphore   to inform when the community is created.
	 */
	private static <T extends CommunitiesRepository> void createNextCommunity(T repository, List<Community> communities,
			int tries, Semaphore semaphore) {

		final Community community = new CommunityTest().createModelExample(communities.size());
		repository.storeCommunity(community, stored -> {
			if (!stored.failed()) {

				communities.add(stored.result());

			}
			if (tries > 1) {
				createNextCommunity(repository, communities, tries - 1, semaphore);
			}
			semaphore.release();
		});

	}

	/**
	 * Verify that can find all the communities.
	 *
	 * @param pool        that create the mongo connections.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunitiesPageObject(String, String, List,
	 *      String, Long, Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindAllCommunities(MongoClient pool, VertxTestContext testContext) {

		removeAllCommunities(pool);
		final List<Community> communities = createAndStoreSomeCommunities(CommunitiesRepositoryTestCase.this.repository,
				23);

		final String name = null;
		final String description = null;
		final List<String> keywords = null;
		final String avatar = null;
		final Long sinceFrom = null;
		final Long sinceTo = null;
		final int offset = 0;
		final int limit = 100;
		CommunitiesRepositoryTestCase.this.repository.searchCommunitiesPageObject(name, description, keywords, avatar,
				sinceFrom, sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final CommunitiesPage foundPage = Model.fromJsonObject(found, CommunitiesPage.class);
					assertThat(foundPage.offset).isEqualTo(offset);
					assertThat(foundPage.total).isEqualTo(23);
					assertThat(foundPage.communities).isEqualTo(communities);

					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can find some communities on a range.
	 *
	 * @param pool        that create the mongo connections.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunitiesPageObject(String, String, List,
	 *      String, Long, Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindCommunitiesInARange(MongoClient pool, VertxTestContext testContext) {

		removeAllCommunities(pool);
		final List<Community> communities = createAndStoreSomeCommunities(CommunitiesRepositoryTestCase.this.repository,
				23);

		final String name = null;
		final String description = null;
		final List<String> keywords = null;
		final String avatar = null;
		final Long sinceFrom = null;
		final Long sinceTo = null;
		final int offset = 5;
		final int limit = 10;
		CommunitiesRepositoryTestCase.this.repository.searchCommunitiesPageObject(name, description, keywords, avatar,
				sinceFrom, sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final CommunitiesPage foundPage = Model.fromJsonObject(found, CommunitiesPage.class);
					assertThat(foundPage.offset).isEqualTo(offset);
					assertThat(foundPage.total).isEqualTo(23);
					assertThat(foundPage.communities).isEqualTo(communities.subList(5, 15));

					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can find some communities with a specific name.
	 *
	 * @param pool        that create the mongo connections.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunitiesPageObject(String, String, List,
	 *      String, Long, Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindCommunitiesWithAName(MongoClient pool, VertxTestContext testContext) {

		removeAllCommunities(pool);
		final List<Community> communities = createAndStoreSomeCommunities(CommunitiesRepositoryTestCase.this.repository,
				23);

		final String name = ".+1\\d";
		final String description = null;
		final List<String> keywords = null;
		final String avatar = null;
		final Long sinceFrom = null;
		final Long sinceTo = null;
		final int offset = 0;
		final int limit = 100;
		CommunitiesRepositoryTestCase.this.repository.searchCommunitiesPageObject(name, description, keywords, avatar,
				sinceFrom, sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final CommunitiesPage foundPage = Model.fromJsonObject(found, CommunitiesPage.class);
					assertThat(foundPage.offset).isEqualTo(0);
					assertThat(foundPage.total).isEqualTo(10);
					assertThat(foundPage.communities).isEqualTo(communities.subList(10, 20));

					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can find some communities with a specific description.
	 *
	 * @param pool        that create the mongo connections.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunitiesPageObject(String, String, List,
	 *      String, Long, Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindCommunitiesWithADescription(MongoClient pool, VertxTestContext testContext) {

		removeAllCommunities(pool);
		final List<Community> communities = createAndStoreSomeCommunities(CommunitiesRepositoryTestCase.this.repository,
				23);

		final String name = null;
		final String description = ".+2\\d";
		final List<String> keywords = null;
		final String avatar = null;
		final Long sinceFrom = null;
		final Long sinceTo = null;
		final int offset = 0;
		final int limit = 100;
		CommunitiesRepositoryTestCase.this.repository.searchCommunitiesPageObject(name, description, keywords, avatar,
				sinceFrom, sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final CommunitiesPage foundPage = Model.fromJsonObject(found, CommunitiesPage.class);
					assertThat(foundPage.offset).isEqualTo(0);
					assertThat(foundPage.total).isEqualTo(3);
					assertThat(foundPage.communities).isEqualTo(communities.subList(20, 23));

					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can find some communities with a specific keywords.
	 *
	 * @param pool        that create the mongo connections.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunitiesPageObject(String, String, List,
	 *      String, Long, Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindCommunitiesWithKeywords(MongoClient pool, VertxTestContext testContext) {

		removeAllCommunities(pool);
		final List<Community> communities = createAndStoreSomeCommunities(CommunitiesRepositoryTestCase.this.repository,
				23);

		final String name = null;
		final String description = null;
		final List<String> keywords = new ArrayList<>();
		keywords.add("keyword 19");
		keywords.add("keyword 21");
		final String avatar = null;
		final Long sinceFrom = null;
		final Long sinceTo = null;
		final int offset = 0;
		final int limit = 100;
		CommunitiesRepositoryTestCase.this.repository.searchCommunitiesPageObject(name, description, keywords, avatar,
				sinceFrom, sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final CommunitiesPage foundPage = Model.fromJsonObject(found, CommunitiesPage.class);
					assertThat(foundPage.offset).isEqualTo(0);
					assertThat(foundPage.total).isEqualTo(2);
					assertThat(foundPage.communities).isEqualTo(communities.subList(20, 22));

					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can find some communities with a one keyword.
	 *
	 * @param pool        that create the mongo connections.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunitiesPageObject(String, String, List,
	 *      String, Long, Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindCommunitiesWithKeyword(MongoClient pool, VertxTestContext testContext) {

		removeAllCommunities(pool);
		final List<Community> communities = createAndStoreSomeCommunities(CommunitiesRepositoryTestCase.this.repository,
				23);

		final String name = null;
		final String description = null;
		final List<String> keywords = new ArrayList<>();
		keywords.add("keyword 19");
		final String avatar = null;
		final Long sinceFrom = null;
		final Long sinceTo = null;
		final int offset = 0;
		final int limit = 100;
		CommunitiesRepositoryTestCase.this.repository.searchCommunitiesPageObject(name, description, keywords, avatar,
				sinceFrom, sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final CommunitiesPage foundPage = Model.fromJsonObject(found, CommunitiesPage.class);
					assertThat(foundPage.offset).isEqualTo(0);
					assertThat(foundPage.total).isEqualTo(4);
					assertThat(foundPage.communities).isEqualTo(communities.subList(18, 22));

					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can find some communities with a specific avatar.
	 *
	 * @param pool        that create the mongo connections.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunitiesPageObject(String, String, List,
	 *      String, Long, Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindCommunitiesWithAnAvatar(MongoClient pool, VertxTestContext testContext) {

		removeAllCommunities(pool);
		final List<Community> communities = createAndStoreSomeCommunities(CommunitiesRepositoryTestCase.this.repository,
				23);

		final String name = null;
		final String description = null;
		final List<String> keywords = null;
		final String avatar = ".+r\\d\\.png";
		final Long sinceFrom = null;
		final Long sinceTo = null;
		final int offset = 0;
		final int limit = 100;
		CommunitiesRepositoryTestCase.this.repository.searchCommunitiesPageObject(name, description, keywords, avatar,
				sinceFrom, sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final CommunitiesPage foundPage = Model.fromJsonObject(found, CommunitiesPage.class);
					assertThat(foundPage.offset).isEqualTo(0);
					assertThat(foundPage.total).isEqualTo(10);
					assertThat(foundPage.communities).isEqualTo(communities.subList(0, 10));

					testContext.completeNow();
				})));

	}

	/**
	 * Create an aggregate some communities with a fake {@link Community#sinceTime}.
	 *
	 * @param pool that create the mongo connections.
	 * @param max  number of communities to try to create.
	 *
	 * @return the aggregated communities.
	 */
	public static List<Community> createAndStoreSomeCommunitiesWithFakeSinceTime(MongoClient pool, int max) {

		final List<Community> communities = new ArrayList<>();
		final Semaphore semaphore = new Semaphore(0);
		createNextCommunityWithFakeSinceTime(pool, communities, max, semaphore);

		try {
			semaphore.acquire(max);
		} catch (final InterruptedException ignored) {
		}

		return communities;

	}

	/**
	 * Create an store a community with a fake since time.
	 *
	 * @param pool        that create the mongo connections.
	 * @param communities that has been created.
	 * @param tries       number maximum of times to create a community.
	 * @param semaphore   to inform when the community is created.
	 */
	private static void createNextCommunityWithFakeSinceTime(MongoClient pool, List<Community> communities, int tries,
			Semaphore semaphore) {

		final int index = communities.size();
		final Community community = new CommunityTest().createModelExample(index);
		community.sinceTime = index * 100000;
		pool.save(CommunitiesRepositoryImpl.COMMUNITIES_COLLECTION, community.toJsonObject(), stored -> {
			if (!stored.failed()) {

				community._id = stored.result();
				communities.add(community);

			}
			if (tries > 1) {
				createNextCommunityWithFakeSinceTime(pool, communities, tries - 1, semaphore);
			}
			semaphore.release();
		});

	}

	/**
	 * Verify that can find some communities since a from time.
	 *
	 * @param pool        that create the mongo connections.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunitiesPageObject(String, String, List,
	 *      String, Long, Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindCommunitiesSinceAFromTime(MongoClient pool, VertxTestContext testContext) {

		removeAllCommunities(pool);
		final List<Community> communities = createAndStoreSomeCommunitiesWithFakeSinceTime(pool, 23);

		final String name = null;
		final String description = null;
		final List<String> keywords = null;
		final String avatar = null;
		final Long sinceFrom = 1500000l;
		final Long sinceTo = null;
		final int offset = 0;
		final int limit = 100;
		CommunitiesRepositoryTestCase.this.repository.searchCommunitiesPageObject(name, description, keywords, avatar,
				sinceFrom, sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final CommunitiesPage foundPage = Model.fromJsonObject(found, CommunitiesPage.class);
					assertThat(foundPage.offset).isEqualTo(0);
					assertThat(foundPage.total).isEqualTo(8);
					assertThat(foundPage.communities).isEqualTo(communities.subList(15, 23));

					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can find some communities since a to time.
	 *
	 * @param pool        that create the mongo connections.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunitiesPageObject(String, String, List,
	 *      String, Long, Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindCommunitiesSinceAToTime(MongoClient pool, VertxTestContext testContext) {

		removeAllCommunities(pool);
		final List<Community> communities = createAndStoreSomeCommunitiesWithFakeSinceTime(pool, 23);

		final String name = null;
		final String description = null;
		final List<String> keywords = null;
		final String avatar = null;
		final Long sinceFrom = null;
		final Long sinceTo = 1500000l;
		final int offset = 0;
		final int limit = 100;
		CommunitiesRepositoryTestCase.this.repository.searchCommunitiesPageObject(name, description, keywords, avatar,
				sinceFrom, sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final CommunitiesPage foundPage = Model.fromJsonObject(found, CommunitiesPage.class);
					assertThat(foundPage.offset).isEqualTo(0);
					assertThat(foundPage.total).isEqualTo(16);
					assertThat(foundPage.communities).isEqualTo(communities.subList(0, 16));

					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can find some communities since time in a range.
	 *
	 * @param pool        that create the mongo connections.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunitiesPageObject(String, String, List,
	 *      String, Long, Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindCommunitiesSinceTimeInARange(MongoClient pool, VertxTestContext testContext) {

		removeAllCommunities(pool);
		final List<Community> communities = createAndStoreSomeCommunitiesWithFakeSinceTime(pool, 23);

		final String name = null;
		final String description = null;
		final List<String> keywords = null;
		final String avatar = null;
		final Long sinceFrom = 1200000l;
		final Long sinceTo = 1800000l;
		final int offset = 0;
		final int limit = 100;
		CommunitiesRepositoryTestCase.this.repository.searchCommunitiesPageObject(name, description, keywords, avatar,
				sinceFrom, sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final CommunitiesPage foundPage = Model.fromJsonObject(found, CommunitiesPage.class);
					assertThat(foundPage.offset).isEqualTo(0);
					assertThat(foundPage.total).isEqualTo(7);
					assertThat(foundPage.communities).isEqualTo(communities.subList(12, 19));

					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can find some communities.
	 *
	 * @param pool        that create the mongo connections.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunitiesPageObject(String, String, List,
	 *      String, Long, Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindCommunities(MongoClient pool, VertxTestContext testContext) {

		removeAllCommunities(pool);
		final List<Community> communities = createAndStoreSomeCommunitiesWithFakeSinceTime(pool, 23);

		final String name = ".+ 1\\d";
		final String description = "3|4|5|6";
		final List<String> keywords = new ArrayList<>();
		keywords.add("\\d{2}");
		final String avatar = ".*png";
		final Long sinceFrom = 1200000l;
		final Long sinceTo = 1800000l;
		final int offset = 1;
		final int limit = 2;
		CommunitiesRepositoryTestCase.this.repository.searchCommunitiesPageObject(name, description, keywords, avatar,
				sinceFrom, sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final CommunitiesPage foundPage = Model.fromJsonObject(found, CommunitiesPage.class);
					assertThat(foundPage.offset).isEqualTo(1);
					assertThat(foundPage.total).isEqualTo(4);
					assertThat(foundPage.communities).isEqualTo(communities.subList(14, 16));

					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can find an empty community page if any community match.
	 *
	 * @param pool        that create the mongo connections.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunitiesPageObject(String, String, List,
	 *      String, Long, Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindEmptyCommunitiesPageIfAnyMatch(MongoClient pool, VertxTestContext testContext) {

		removeAllCommunities(pool);
		final String name = null;
		final String description = null;
		final List<String> keywords = new ArrayList<>();
		final String avatar = null;
		final Long sinceFrom = null;
		final Long sinceTo = null;
		final int offset = 0;
		final int limit = 100;
		CommunitiesRepositoryTestCase.this.repository.searchCommunitiesPageObject(name, description, keywords, avatar,
				sinceFrom, sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final CommunitiesPage foundPage = Model.fromJsonObject(found, CommunitiesPage.class);
					assertThat(foundPage.offset).isEqualTo(0);
					assertThat(foundPage.total).isEqualTo(0);
					assertThat(foundPage.communities).isNull();

					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can find an empty community page if any community match.
	 *
	 * @param pool        that create the mongo connections.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunitiesPageObject(String, String, List,
	 *      String, Long, Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFindEmptyCommunitiesPageIfOffsetIsOutOfRange(MongoClient pool, VertxTestContext testContext) {

		removeAllCommunities(pool);
		createAndStoreSomeCommunities(this.repository, 2);
		final String name = null;
		final String description = null;
		final List<String> keywords = new ArrayList<>();
		final String avatar = null;
		final Long sinceFrom = null;
		final Long sinceTo = null;
		final int offset = 3;
		final int limit = 100;
		CommunitiesRepositoryTestCase.this.repository.searchCommunitiesPageObject(name, description, keywords, avatar,
				sinceFrom, sinceTo, offset, limit, testContext.succeeding(found -> testContext.verify(() -> {

					final CommunitiesPage foundPage = Model.fromJsonObject(found, CommunitiesPage.class);
					assertThat(foundPage.offset).isEqualTo(3);
					assertThat(foundPage.total).isEqualTo(2);
					assertThat(foundPage.communities).isNull();

					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can not found a community member.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMember(String, String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotFoundCommunityMember(VertxTestContext testContext) {

		this.repository.searchCommunityMember("undefined community identifier", "undefined user id",
				testContext.failing(failed -> {
					testContext.completeNow();
				}));

	}

	/**
	 * Verify that can found a community member.
	 *
	 *
	 * @param profileManager service to manage profile managers.
	 * @param testContext    context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMember(String, String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityMember(WeNetProfileManagerService profileManager, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final CommunityMember member = new CommunityMember();
		member.userId = UUID.randomUUID().toString();
		final long now = TimeManager.now();
		this.repository.storeCommunityMember(communityId, member, testContext.succeeding(storedMember -> {
			this.repository.searchCommunityMember(communityId, member.userId,
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
	 *
	 * @param profileManager service to manage profile managers.
	 * @param testContext    context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMemberObject(String, String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityMemberObject(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final String userId = UUID.randomUUID().toString();
		final long now = TimeManager.now();
		this.repository.storeCommunityMemberObject(communityId, new JsonObject().put("userId", userId),
				testContext.succeeding(storedMember -> {
					this.repository.searchCommunityMemberObject(communityId, userId,
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
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunityMember(String,CommunityMember,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotStoreACommunityMemberThatCanNotBeAnObject(VertxTestContext testContext) {

		final CommunityMember communityMember = new CommunityMember() {

			/**
			 * {@inheritDoc}
			 */
			@Override
			public JsonObject toJsonObject() {

				return null;
			}
		};
		this.repository.storeCommunityMember("communityId", communityMember, testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can store a community member.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunityMember(String, CommunityMember,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldStoreCommunityMember(VertxTestContext testContext) {

		final long now = TimeManager.now();
		final CommunityMember communityMember = new CommunityMember();
		this.repository.storeCommunityMember("communityId", communityMember,
				testContext.succeeding(storedCommunityMember -> testContext.verify(() -> {

					assertThat(storedCommunityMember).isNotNull();
					assertThat(storedCommunityMember.joinTime).isGreaterThanOrEqualTo(now);
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can store a community member object.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunityMemberObject(String,JsonObject,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldStoreCommunityMemberObject(VertxTestContext testContext) {

		final long now = TimeManager.now();
		this.repository.storeCommunityMemberObject("communityId", new JsonObject(),
				testContext.succeeding(storedCommunityMember -> testContext.verify(() -> {

					assertThat(storedCommunityMember).isNotNull();
					assertThat(storedCommunityMember.getLong("joinTime", 0l)).isNotEqualTo(0).isGreaterThanOrEqualTo(now);
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that can not update a community member if it is not defined.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunityMember(String,CommunityMember,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateUndefinedCommunityMember(VertxTestContext testContext) {

		final CommunityMember communityMember = new CommunityMember();
		communityMember.userId = "undefined community member identifier";
		this.repository.updateCommunityMember("communityId", communityMember, testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not update a community member if it is not defined.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunityMemberObject(String,JsonObject,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateUndefinedCommunityMemberObject(VertxTestContext testContext) {

		final JsonObject communityMember = new JsonObject().put("userId", "undefined community member identifier");
		this.repository.updateCommunityMemberObject("communityId", communityMember, testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that can not update a community member if it is not defined.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#updateCommunityMember(String,CommunityMember,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateACommunityMemberThatCanNotBeAnObject(VertxTestContext testContext) {

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
		this.repository.updateCommunityMember("communityId", communityMember, testContext.failing(failed -> {
			testContext.completeNow();
		}));

	}

	/**
	 * Verify that update a community member.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#storeCommunityMember(String,CommunityMember,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdateACommunityMember(VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final CommunityMember communityMember = new CommunityMember();
		communityMember.userId = UUID.randomUUID().toString();
		this.repository.storeCommunityMember(communityId, communityMember, testContext.succeeding(stored -> {

			final CommunityMember member = new CommunityMember();
			member.userId = communityMember.userId;
			this.repository.updateCommunityMember(communityId, member, testContext.succeeding(update -> {
				testContext.completeNow();
			}));

		}));

	}

	/**
	 * Verify that delete a community member.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#deleteCommunityMember(String, String,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldDeleteCommunityMember(VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final CommunityMember communityMember = new CommunityMember();
		final String userId = UUID.randomUUID().toString();
		communityMember.userId = userId;
		this.repository.storeCommunityMember(communityId, communityMember, testContext.succeeding(stored -> {

			this.repository.deleteCommunityMember(communityId, userId, testContext.succeeding(delete -> {

				this.repository.searchCommunityMember(communityId, userId, testContext.failing(search -> {
					testContext.completeNow();
				}));

			}));
		}));

	}

	/**
	 * Verify that found an empty community member page.
	 *
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMembersPageObject(String, Long,
	 *      Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundEmptyCommunityMemberPage(VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final CommunityMember communityMember = new CommunityMember();
		final String userId = UUID.randomUUID().toString();
		communityMember.userId = userId;
		this.repository.searchCommunityMembersPageObject(communityId, null, null, 0, 100,
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
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMembersPageObject(String, Long,
	 *      Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityMemberPage(VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		this.repository.storeCommunityMemberObject(communityId,
				new JsonObject().put("userId", UUID.randomUUID().toString()), testContext.succeeding(stored1 -> {

					this.repository.storeCommunityMemberObject(communityId,
							new JsonObject().put("userId", UUID.randomUUID().toString()), testContext.succeeding(stored2 -> {

								this.repository.searchCommunityMembersPageObject(communityId, null, null, 0, 100,
										testContext.succeeding(page -> testContext.verify(() -> {

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
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMembersPageObject(String, Long,
	 *      Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityMemberPageWithJoinFrom(VertxTestContext testContext) {

		final long now = TimeManager.now();
		final String communityId = UUID.randomUUID().toString();
		this.repository.storeCommunityMemberObject(communityId,
				new JsonObject().put("userId", UUID.randomUUID().toString()), testContext.succeeding(stored1 -> {

					this.repository.storeCommunityMemberObject(communityId,
							new JsonObject().put("userId", UUID.randomUUID().toString()), testContext.succeeding(stored2 -> {

								this.repository.searchCommunityMembersPageObject(communityId, now, null, 0, 100,
										testContext.succeeding(page -> testContext.verify(() -> {

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
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMembersPageObject(String, Long,
	 *      Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityMemberPageWithJoinTo(VertxTestContext testContext) {

		final long now = TimeManager.now();
		final String communityId = UUID.randomUUID().toString();
		this.repository.storeCommunityMemberObject(communityId,
				new JsonObject().put("userId", UUID.randomUUID().toString()), testContext.succeeding(stored1 -> {

					this.repository.storeCommunityMemberObject(communityId,
							new JsonObject().put("userId", UUID.randomUUID().toString()), testContext.succeeding(stored2 -> {

								this.repository.searchCommunityMembersPageObject(communityId, null, now + 1, 0, 100,
										testContext.succeeding(page -> testContext.verify(() -> {

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

}
