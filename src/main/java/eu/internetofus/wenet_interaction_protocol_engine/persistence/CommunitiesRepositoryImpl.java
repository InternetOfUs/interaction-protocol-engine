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

import eu.internetofus.common.TimeManager;
import eu.internetofus.common.persitences.Repository;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;

/**
 * Implementation of the {@link CommunitiesRepository}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class CommunitiesRepositoryImpl extends Repository implements CommunitiesRepository {

	/**
	 * The name of the collection that contains the communities.
	 */
	public static final String COMMUNITIES_COLLECTION = "communities";

	/**
	 * The name of the collection that contains the community members.
	 */
	public static final String COMMUNITY_MEMBERS_COLLECTION = "communityMembers";

	/**
	 * The name of the collection that contains the community norms.
	 */
	public static final String COMMUNITY_NORMS_COLLECTION = "communityNorms";

	/**
	 * Create a new service.
	 *
	 * @param pool to create the connections.
	 */
	public CommunitiesRepositoryImpl(MongoClient pool) {

		super(pool);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void searchCommunityObject(String id, Handler<AsyncResult<JsonObject>> searchHandler) {

		final JsonObject query = new JsonObject().put("_id", id);
		this.findOneDocument(COMMUNITIES_COLLECTION, query, null, null, searchHandler);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void storeCommunity(JsonObject community, Handler<AsyncResult<JsonObject>> storeHandler) {

		final long now = TimeManager.now();
		community.put("sinceTime", now);
		this.storeOneDocument(COMMUNITIES_COLLECTION, community, null, storeHandler);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void updateCommunity(JsonObject community, Handler<AsyncResult<Void>> updateHandler) {

		final String id = community.getString("_id");
		final JsonObject query = new JsonObject().put("_id", id);
		this.updateOneDocument(COMMUNITIES_COLLECTION, query, community, updateHandler);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void deleteCommunity(String id, Handler<AsyncResult<Void>> deleteHandler) {

		final JsonObject query = new JsonObject().put("_id", id);
		this.deleteOneDocument(COMMUNITIES_COLLECTION, query, deleteHandler);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void searchCommunityPageObject(JsonObject query, int offset, int limit,
			Handler<AsyncResult<JsonObject>> searchHandler) {

		this.searchPageObject(COMMUNITIES_COLLECTION, query, null, offset, limit, "communities", searchHandler);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void searchCommunityMemberObject(String communityId, String userId,
			Handler<AsyncResult<JsonObject>> searchHandler) {

		final JsonObject query = new JsonObject().put("communityId", communityId).put("userId", userId);
		final JsonObject fields = new JsonObject().put("_id", 0).put("communityId", 0);
		this.findOneDocument(COMMUNITY_MEMBERS_COLLECTION, query, fields, null, searchHandler);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void storeCommunityMemberObject(String communityId, JsonObject member,
			Handler<AsyncResult<JsonObject>> storeHandler) {

		final long now = TimeManager.now();
		member.put("joinTime", now);
		member.put("communityId", communityId);
		this.storeOneDocument(COMMUNITY_MEMBERS_COLLECTION, member, null, storeHandler, "_id", "communityId");

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void updateCommunityMemberObject(String communityId, JsonObject member,
			Handler<AsyncResult<Void>> updateHandler) {

		final String userId = member.getString("userId");
		final JsonObject query = new JsonObject().put("communityId", communityId).put("userId", userId);
		this.updateOneDocument(COMMUNITY_MEMBERS_COLLECTION, query, member, updateHandler);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void deleteCommunityMember(String communityId, String userId, Handler<AsyncResult<Void>> deleteHandler) {

		final JsonObject query = new JsonObject().put("communityId", communityId).put("userId", userId);
		this.deleteOneDocument(COMMUNITY_MEMBERS_COLLECTION, query, deleteHandler);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void searchCommunityMembersPageObject(String communityId, JsonObject query, int offset, int limit,
			Handler<AsyncResult<JsonObject>> searchHandler) {

		query.put("communityId", communityId);
		this.searchPageObject(COMMUNITY_MEMBERS_COLLECTION, query, new JsonObject().put("communityId", 0).put("_id", 0),
				offset, limit, "members", searchHandler);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void searchCommunityNormObject(String communityId, String normId,
			Handler<AsyncResult<JsonObject>> searchHandler) {

		final JsonObject query = new JsonObject().put("communityId", communityId).put("_id", normId);
		final JsonObject fields = new JsonObject().put("communityId", 0);
		this.findOneDocument(COMMUNITY_NORMS_COLLECTION, query, fields, null, searchHandler);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void storeCommunityNormObject(String communityId, JsonObject norm,
			Handler<AsyncResult<JsonObject>> storeHandler) {

		final long now = TimeManager.now();
		norm.put("sinceTime", now);
		norm.put("communityId", communityId);
		this.storeOneDocument(COMMUNITY_NORMS_COLLECTION, norm, null, storeHandler, "communityId");

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void deleteCommunityNorm(String communityId, String normId, Handler<AsyncResult<Void>> deleteHandler) {

		final JsonObject query = new JsonObject().put("communityId", communityId).put("_id", normId);
		this.deleteOneDocument(COMMUNITY_NORMS_COLLECTION, query, deleteHandler);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void searchCommunityNormsPageObject(String communityId, JsonObject query, int offset, int limit,
			Handler<AsyncResult<JsonObject>> searchHandler) {

		query.put("communityId", communityId);
		this.searchPageObject(COMMUNITY_NORMS_COLLECTION, query, new JsonObject().put("communityId", 0), offset, limit,
				"norms", searchHandler);

	}

}
