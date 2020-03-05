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

import java.util.List;

import eu.internetofus.wenet_interaction_protocol_engine.Model;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.Community;
import eu.internetofus.wenet_interaction_protocol_engine.api.communities.CommunityMember;
import io.vertx.codegen.annotations.GenIgnore;
import io.vertx.codegen.annotations.ProxyGen;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.serviceproxy.ServiceBinder;

/**
 * The service to manage the {@link Community} on the database.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ProxyGen
public interface CommunitiesRepository {

	/**
	 * The address of this service.
	 */
	String ADDRESS = "wenet_interaction_protocol_engine.persistence.communities";

	/**
	 * Register this service.
	 *
	 * @param vertx that contains the event bus to use.
	 * @param pool  to create the database connections.
	 */
	static void register(Vertx vertx, MongoClient pool) {

		new ServiceBinder(vertx).setAddress(CommunitiesRepository.ADDRESS).register(CommunitiesRepository.class,
				new CommunitiesRepositoryImpl(pool));

	}

	/**
	 * Create a proxy of the {@link CommunitiesRepository}.
	 *
	 * @param vertx where the service has to be used.
	 *
	 * @return the community.
	 */
	static CommunitiesRepository createProxy(Vertx vertx) {

		return new CommunitiesRepositoryVertxEBProxy(vertx, CommunitiesRepository.ADDRESS);
	}

	/**
	 * Search for the community with the specified identifier.
	 *
	 * @param id            identifier of the community to search.
	 * @param searchHandler handler to manage the search.
	 */
	@GenIgnore
	default void searchCommunity(String id, Handler<AsyncResult<Community>> searchHandler) {

		this.searchCommunityObject(id, search -> {

			if (search.failed()) {

				searchHandler.handle(Future.failedFuture(search.cause()));

			} else {

				final JsonObject value = search.result();
				final Community community = Model.fromJsonObject(value, Community.class);
				if (community == null) {

					searchHandler.handle(Future.failedFuture("The stored community is not valid."));

				} else {

					searchHandler.handle(Future.succeededFuture(community));
				}
			}
		});
	}

	/**
	 * Search for the community with the specified identifier.
	 *
	 * @param id            identifier of the community to search.
	 * @param searchHandler handler to manage the search.
	 */
	void searchCommunityObject(String id, Handler<AsyncResult<JsonObject>> searchHandler);

	/**
	 * Store a community.
	 *
	 * @param community    to store.
	 * @param storeHandler handler to manage the store.
	 */
	@GenIgnore
	default void storeCommunity(Community community, Handler<AsyncResult<Community>> storeHandler) {

		final JsonObject object = community.toJsonObject();
		if (object == null) {

			storeHandler.handle(Future.failedFuture("The community can not converted to JSON."));

		} else {

			this.storeCommunity(object, stored -> {
				if (stored.failed()) {

					storeHandler.handle(Future.failedFuture(stored.cause()));

				} else {

					final JsonObject value = stored.result();
					final Community storedCommunity = Model.fromJsonObject(value, Community.class);
					if (storedCommunity == null) {

						storeHandler.handle(Future.failedFuture("The stored community is not valid."));

					} else {

						storeHandler.handle(Future.succeededFuture(storedCommunity));
					}

				}
			});
		}
	}

	/**
	 * Store a community.
	 *
	 * @param community    to store.
	 * @param storeHandler handler to manage the search.
	 */
	void storeCommunity(JsonObject community, Handler<AsyncResult<JsonObject>> storeHandler);

	/**
	 * Update a community.
	 *
	 * @param community     to update.
	 * @param updateHandler handler to manage the update.
	 */
	@GenIgnore
	default void updateCommunity(Community community, Handler<AsyncResult<Community>> updateHandler) {

		final JsonObject object = community.toJsonObject();
		if (object == null) {

			updateHandler.handle(Future.failedFuture("The community can not converted to JSON."));

		} else {

			this.updateCommunity(object, updated -> {
				if (updated.failed()) {

					updateHandler.handle(Future.failedFuture(updated.cause()));

				} else {

					final JsonObject value = updated.result();
					final Community updatedCommunity = Model.fromJsonObject(value, Community.class);
					if (updatedCommunity == null) {

						updateHandler.handle(Future.failedFuture("The updated community is not valid."));

					} else {

						updateHandler.handle(Future.succeededFuture(updatedCommunity));
					}

				}
			});
		}
	}

	/**
	 * Update a community.
	 *
	 * @param community     to update.
	 * @param updateHandler handler to manage the update result.
	 */
	void updateCommunity(JsonObject community, Handler<AsyncResult<JsonObject>> updateHandler);

	/**
	 * Delete a community.
	 *
	 * @param id            identifier of the community to delete.
	 * @param deleteHandler handler to manage the delete result.
	 */
	void deleteCommunity(String id, Handler<AsyncResult<Void>> deleteHandler);

	/**
	 * Search for the communities that satisfy the query.
	 *
	 * @param name          of the communities to return or a Perl compatible
	 *                      regular expressions (PCRE) that has to match the name of
	 *                      the communities to return.
	 * @param description   of the communities to return or a Perl compatible
	 *                      regular expressions (PCRE) that has to match the
	 *                      description of the communities to return.
	 * @param keywords      of the communities to return or a Perl compatible
	 *                      regular expressions (PCRE) that has to match the keyword
	 *                      of the communities to return.
	 * @param avatar        of the communities to return or a Perl compatible
	 *                      regular expressions (PCRE) that has to match the avatar
	 *                      of the communities to return.
	 * @param sinceFrom     time stamp inclusive that mark the older limit in witch
	 *                      the community has been created. It is the difference,
	 *                      measured in seconds, between the time when the community
	 *                      was created and midnight, January 1, 1970 UTC.
	 * @param sinceTo       time stamp inclusive that mark the newest limit in witch
	 *                      the community has been created. It is the difference,
	 *                      measured in seconds, between the time when the community
	 *                      was created and midnight, January 1, 1970 UTC.
	 * @param offset        index of the first community to return.
	 * @param limit         number maximum of communities to return.
	 * @param searchHandler handler to manage the search.
	 */
	@GenIgnore
	default void searchCommunitiesPageObject(String name, String description, List<String> keywords, String avatar,
			Long sinceFrom, Long sinceTo, int offset, int limit, Handler<AsyncResult<JsonObject>> searchHandler) {

		final JsonObject query = new QueryBuilder().withRegex("name", name).withRegex("description", description)
				.withRegex("keywords", keywords).withRegex("avatar", avatar).withRange("sinceTime", sinceFrom, sinceTo).build();
		this.searchCommunityPageObject(query, offset, limit, searchHandler);

	}

	/**
	 * Search for the communities that satisfy the query.
	 *
	 * @param query         that has to match the communities to search.
	 * @param offset        index of the first community to return.
	 * @param limit         number maximum of communities to return.
	 * @param searchHandler handler to manage the search.
	 */
	void searchCommunityPageObject(JsonObject query, int offset, int limit,
			Handler<AsyncResult<JsonObject>> searchHandler);

	/**
	 * Search for a community member.
	 *
	 * @param communityId   identifier of the community to get the member.
	 * @param userId        identifier of the member to get
	 * @param searchHandler handler to manage the search.
	 */
	@GenIgnore
	default void searchCommunityMember(String communityId, String userId,
			Handler<AsyncResult<CommunityMember>> searchHandler) {

		this.searchCommunityMemberObject(communityId, userId, search -> {

			if (search.failed()) {

				searchHandler.handle(Future.failedFuture(search.cause()));

			} else {

				final JsonObject value = search.result();
				final CommunityMember member = Model.fromJsonObject(value, CommunityMember.class);
				if (member == null) {

					searchHandler.handle(Future.failedFuture("The stored community is not valid."));

				} else {

					searchHandler.handle(Future.succeededFuture(member));
				}
			}
		});
	}

	/**
	 * Search for the community member object.
	 *
	 * @param communityId   identifier of the community to get the member.
	 * @param userId        identifier of the member to get
	 * @param searchHandler handler to manage the search.
	 */
	void searchCommunityMemberObject(String communityId, String userId, Handler<AsyncResult<JsonObject>> searchHandler);

	/**
	 * Store a community member.
	 *
	 * @param communityId  identifier of the community to store the member.
	 * @param member       community member to store.
	 * @param storeHandler handler to manage the store.
	 */
	@GenIgnore
	default void storeCommunityMember(String communityId, CommunityMember member,
			Handler<AsyncResult<CommunityMember>> storeHandler) {

		final JsonObject object = member.toJsonObject();
		if (object == null) {

			storeHandler.handle(Future.failedFuture("The community member can not converted to JSON."));

		} else {

			this.storeCommunityMemberObject(communityId, object, stored -> {
				if (stored.failed()) {

					storeHandler.handle(Future.failedFuture(stored.cause()));

				} else {

					final JsonObject value = stored.result();
					final CommunityMember storedCommunityMember = Model.fromJsonObject(value, CommunityMember.class);
					if (storedCommunityMember == null) {

						storeHandler.handle(Future.failedFuture("The stored community member is not valid."));

					} else {

						storeHandler.handle(Future.succeededFuture(storedCommunityMember));
					}

				}
			});
		}
	}

	/**
	 * Store a community.
	 *
	 * @param communityId  identifier of the community to store the member.
	 * @param member       to store.
	 * @param storeHandler handler to manage the search.
	 */
	void storeCommunityMemberObject(String communityId, JsonObject member, Handler<AsyncResult<JsonObject>> storeHandler);

	/**
	 * Update a community member.
	 *
	 * @param communityId   identifier of the community to update the member.
	 * @param member        to update.
	 * @param updateHandler handler to manage the update.
	 */
	@GenIgnore
	default void updateCommunityMember(String communityId, CommunityMember member,
			Handler<AsyncResult<CommunityMember>> updateHandler) {

		final JsonObject object = member.toJsonObject();
		if (object == null) {

			updateHandler.handle(Future.failedFuture("The community member can not converted to JSON."));

		} else {

			this.updateCommunityMemberObject(communityId, object, updated -> {
				if (updated.failed()) {

					updateHandler.handle(Future.failedFuture(updated.cause()));

				} else {

					final JsonObject value = updated.result();
					final CommunityMember updatedCommunityMember = Model.fromJsonObject(value, CommunityMember.class);
					if (updatedCommunityMember == null) {

						updateHandler.handle(Future.failedFuture("The updated community member is not valid."));

					} else {

						updateHandler.handle(Future.succeededFuture(updatedCommunityMember));
					}

				}
			});
		}
	}

	/**
	 * Update a community member.
	 *
	 * @param communityId   identifier of the community to update the member.
	 * @param member        to update.
	 * @param updateHandler handler to manage the update result.
	 */
	void updateCommunityMemberObject(String communityId, JsonObject member,
			Handler<AsyncResult<JsonObject>> updateHandler);

	/**
	 * Delete a community member.
	 *
	 * @param communityId   identifier of the community to delete the member.
	 * @param userId        identifier of the user to delete as member to delete.
	 * @param deleteHandler handler to manage the delete result.
	 */
	void deleteCommunityMember(String communityId, String userId, Handler<AsyncResult<Void>> deleteHandler);

	/**
	 * Search for the community members that satisfy the query.
	 *
	 * @param communityId   identifier of the community to get the members.
	 * @param joinFrom      the time stamp inclusive that mark the older limit in
	 *                      witch the community member has joined. It is the
	 *                      difference, measured in seconds, between the time when
	 *                      the member has joined into the community and midnight,
	 *                      January 1, 1970 UTC.
	 * @param joinTo        the time stamp inclusive that mark the newest limit in
	 *                      witch the community member has joined. It is the
	 *                      difference, measured in seconds, between the time when
	 *                      the member has joined into the community and midnight,
	 *                      January 1, 1970 UTC.
	 * @param offset        index of the first community member to return.
	 * @param limit         number maximum of community members to return.
	 * @param searchHandler handler to manage the search.
	 */
	@GenIgnore
	default void searchCommunityMembersPageObject(String communityId, Long joinFrom, Long joinTo, int offset, int limit,
			Handler<AsyncResult<JsonObject>> searchHandler) {

		final JsonObject query = new JsonObject();
		if (joinFrom != null || joinTo != null) {

			final JsonObject restriction = new JsonObject();
			if (joinFrom != null) {

				restriction.put("$gte", joinFrom);

			}
			if (joinTo != null) {

				restriction.put("$lte", joinTo);

			}

			query.put("joinTime", restriction);

		}
		this.searchCommunityMembersPageObject(communityId, query, offset, limit, searchHandler);
	}

	/**
	 * Search for the communities that satisfy the query.
	 *
	 * @param communityId   identifier of the community to get the members.
	 * @param query         that has to match the communities to search.
	 * @param offset        index of the first community to return.
	 * @param limit         number maximum of communities to return.
	 * @param searchHandler handler to manage the search.
	 */
	void searchCommunityMembersPageObject(String communityId, JsonObject query, int offset, int limit,
			Handler<AsyncResult<JsonObject>> searchHandler);

}
