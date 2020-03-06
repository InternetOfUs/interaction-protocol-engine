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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.tinylog.Level;
import org.tinylog.provider.InternalLogger;

import eu.internetofus.wenet_interaction_protocol_engine.Model;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.CommunityMember;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.CommunityMembersPage;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.CommunityNorm;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.CommunityNormsPage;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.junit5.VertxTestContext;

/**
 * Test the {@link CommunitiesRepositoryImpl}.
 *
 * @see CommunitiesRepositoryImpl
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class CommunitiesRepositoryImplTest extends CommunitiesRepositoryTestCase<CommunitiesRepositoryImpl> {

	/**
	 * Create the repository to use in the tests.
	 *
	 * @param pool that create the mongo connections.
	 */
	@BeforeEach
	public void createRepository(MongoClient pool) {

		this.repository = new CommunitiesRepositoryImpl(pool);

	}

	/**
	 * Create some community members
	 *
	 * @param communityId identifier of the community to add the members.
	 * @param pool        connection to the mongo database.
	 * @param max         number of members to create.
	 *
	 * @return the community members with a fake join time.
	 */
	public static List<CommunityMember> createCommunityMembersWithFakeJoinTime(String communityId, MongoClient pool,
			int max) {

		final List<CommunityMember> members = new ArrayList<>();
		final Semaphore semaphore = new Semaphore(0);

		createNextMember(semaphore, members, communityId, pool, max);

		try {
			semaphore.acquire(max);
		} catch (final InterruptedException ignored) {
		}

		return members;

	}

	/**
	 * Create and store the next community member.
	 *
	 * @param semaphore   to inform when the member is created.
	 * @param members     list to store the created member.
	 * @param communityId identifier of the community to add the members.
	 * @param pool        connection to the mongo database.
	 * @param max         number of members to create.
	 */
	private static void createNextMember(Semaphore semaphore, List<CommunityMember> members, String communityId,
			MongoClient pool, int max) {

		final String userId = UUID.randomUUID().toString();
		final long joinTime = members.size() * 100000;
		pool.save(CommunitiesRepositoryImpl.COMMUNITY_MEMBERS_COLLECTION,
				new JsonObject().put("communityId", communityId).put("userId", userId).put("joinTime", joinTime), save -> {

					if (save.failed()) {

						InternalLogger.log(Level.ERROR, save.cause());
					}
					final CommunityMember member = new CommunityMember();
					member.userId = userId;
					member.joinTime = joinTime;
					members.add(member);
					if (members.size() < max) {

						createNextMember(semaphore, members, communityId, pool, max);
					}
					semaphore.release();
				});

	}

	/**
	 * Verify that found some community user members with a join from.
	 *
	 * @param pool        connection to the mongo database.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMembersPageObject(String, Long,
	 *      Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityMemberPageWithJoinFrom(MongoClient pool, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final List<CommunityMember> members = createCommunityMembersWithFakeJoinTime(communityId, pool, 23);
		this.repository.searchCommunityMembersPageObject(communityId, 700000L, null, 0, 100,
				testContext.succeeding(page -> testContext.verify(() -> {

					final CommunityMembersPage pageModel = Model.fromJsonObject(page, CommunityMembersPage.class);
					assertThat(pageModel.offset).isEqualTo(0L);
					assertThat(pageModel.total).isEqualTo(16L);
					assertThat(pageModel.members).isEqualTo(members.subList(7, 23));
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that found some community user members with a join to.
	 *
	 * @param pool        connection to the mongo database.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMembersPageObject(String, Long,
	 *      Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityMemberPageWithJoinTo(MongoClient pool, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final List<CommunityMember> members = createCommunityMembersWithFakeJoinTime(communityId, pool, 23);
		this.repository.searchCommunityMembersPageObject(communityId, null, 700000L, 0, 100,
				testContext.succeeding(page -> testContext.verify(() -> {

					final CommunityMembersPage pageModel = Model.fromJsonObject(page, CommunityMembersPage.class);
					assertThat(pageModel.offset).isEqualTo(0L);
					assertThat(pageModel.total).isEqualTo(8L);
					assertThat(pageModel.members).isEqualTo(members.subList(0, 8));
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that found some community user members with a join range.
	 *
	 * @param pool        connection to the mongo database.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityMembersPageObject(String, Long,
	 *      Long, int, int, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityMemberPageWithJoinRange(MongoClient pool, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final List<CommunityMember> members = createCommunityMembersWithFakeJoinTime(communityId, pool, 23);
		this.repository.searchCommunityMembersPageObject(communityId, 700000L, 1700000L, 1, 5,
				testContext.succeeding(page -> testContext.verify(() -> {

					final CommunityMembersPage pageModel = Model.fromJsonObject(page, CommunityMembersPage.class);
					assertThat(pageModel.offset).isEqualTo(1L);
					assertThat(pageModel.total).isEqualTo(11L);
					assertThat(pageModel.members).isEqualTo(members.subList(8, 13));
					testContext.completeNow();
				})));

	}

	/**
	 * Create some community norms
	 *
	 * @param communityId identifier of the community to add the norms.
	 * @param pool        connection to the mongo database.
	 * @param max         number of norms to create.
	 *
	 * @return the community norms with a fake since time.
	 */
	public static List<CommunityNorm> createCommunityNormsWithFakeSinceTime(String communityId, MongoClient pool,
			int max) {

		final List<CommunityNorm> norms = new ArrayList<>();
		final Semaphore semaphore = new Semaphore(0);

		createNextNorm(semaphore, norms, communityId, pool, max);

		try {
			semaphore.acquire(max);
		} catch (final InterruptedException ignored) {
		}

		return norms;

	}

	/**
	 * Create and store the next community norm.
	 *
	 * @param semaphore   to inform when the norm is created.
	 * @param norms       list to store the created norm.
	 * @param communityId identifier of the community to add the norms.
	 * @param pool        connection to the mongo database.
	 * @param max         number of norms to create.
	 */
	private static void createNextNorm(Semaphore semaphore, List<CommunityNorm> norms, String communityId,
			MongoClient pool, int max) {

		final String userId = UUID.randomUUID().toString();
		final long sinceTime = norms.size() * 100000;
		pool.save(CommunitiesRepositoryImpl.COMMUNITY_NORMS_COLLECTION,
				new JsonObject().put("communityId", communityId).put("_id", userId).put("sinceTime", sinceTime), save -> {

					if (save.failed()) {

						InternalLogger.log(Level.ERROR, save.cause());
					}
					final CommunityNorm norm = new CommunityNorm();
					norm._id = userId;
					norm.sinceTime = sinceTime;
					norms.add(norm);
					if (norms.size() < max) {

						createNextNorm(semaphore, norms, communityId, pool, max);
					}
					semaphore.release();
				});

	}

	/**
	 * Verify that found some community user norms with a since from.
	 *
	 * @param pool        connection to the mongo database.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityNormsPageObject(String, Long, Long,
	 *      int, int, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityNormPageWithSinceFrom(MongoClient pool, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final List<CommunityNorm> norms = createCommunityNormsWithFakeSinceTime(communityId, pool, 23);
		this.repository.searchCommunityNormsPageObject(communityId, 700000L, null, 0, 100,
				testContext.succeeding(page -> testContext.verify(() -> {

					final CommunityNormsPage pageModel = Model.fromJsonObject(page, CommunityNormsPage.class);
					assertThat(pageModel.offset).isEqualTo(0L);
					assertThat(pageModel.total).isEqualTo(16L);
					assertThat(pageModel.norms).isEqualTo(norms.subList(7, 23));
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that found some community user norms with a since to.
	 *
	 * @param pool        connection to the mongo database.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityNormsPageObject(String, Long, Long,
	 *      int, int, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityNormPageWithSinceTo(MongoClient pool, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final List<CommunityNorm> norms = createCommunityNormsWithFakeSinceTime(communityId, pool, 23);
		this.repository.searchCommunityNormsPageObject(communityId, null, 700000L, 0, 100,
				testContext.succeeding(page -> testContext.verify(() -> {

					final CommunityNormsPage pageModel = Model.fromJsonObject(page, CommunityNormsPage.class);
					assertThat(pageModel.offset).isEqualTo(0L);
					assertThat(pageModel.total).isEqualTo(8L);
					assertThat(pageModel.norms).isEqualTo(norms.subList(0, 8));
					testContext.completeNow();
				})));

	}

	/**
	 * Verify that found some community user norms with a since range.
	 *
	 * @param pool        connection to the mongo database.
	 * @param testContext context that executes the test.
	 *
	 * @see CommunitiesRepository#searchCommunityNormsPageObject(String, Long, Long,
	 *      int, int, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunityNormPageWithSinceRange(MongoClient pool, VertxTestContext testContext) {

		final String communityId = UUID.randomUUID().toString();
		final List<CommunityNorm> norms = createCommunityNormsWithFakeSinceTime(communityId, pool, 23);
		this.repository.searchCommunityNormsPageObject(communityId, 700000L, 1700000L, 1, 5,
				testContext.succeeding(page -> testContext.verify(() -> {

					final CommunityNormsPage pageModel = Model.fromJsonObject(page, CommunityNormsPage.class);
					assertThat(pageModel.offset).isEqualTo(1L);
					assertThat(pageModel.total).isEqualTo(11L);
					assertThat(pageModel.norms).isEqualTo(norms.subList(8, 13));
					testContext.completeNow();
				})));

	}

}
