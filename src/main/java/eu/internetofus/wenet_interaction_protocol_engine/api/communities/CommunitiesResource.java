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

package eu.internetofus.wenet_interaction_protocol_engine.api.communities;

import javax.ws.rs.core.Response.Status;

import org.tinylog.Logger;

import eu.internetofus.wenet_interaction_protocol_engine.Model;
import eu.internetofus.wenet_interaction_protocol_engine.ValidationErrorException;
import eu.internetofus.wenet_interaction_protocol_engine.api.OperationReponseHandlers;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.CommunitiesRepository;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.OperationRequest;
import io.vertx.ext.web.api.OperationResponse;

/**
 * Implements the services defined in the {@link Communities}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class CommunitiesResource implements Communities {

	/**
	 * The repository to manage the communities.
	 */
	protected CommunitiesRepository repository;

	/**
	 * Create an empty resource. This is only used for unit tests.
	 */
	protected CommunitiesResource() {

	}

	/**
	 * Create a new instance to provide the services of the {@link Communities}.
	 *
	 * @param vertx where resource is defined.
	 */
	public CommunitiesResource(Vertx vertx) {

		this.repository = CommunitiesRepository.createProxy(vertx);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void createCommunity(JsonObject body, OperationRequest context,
			Handler<AsyncResult<OperationResponse>> resultHandler) {

		final Community community = Model.fromJsonObject(body, Community.class);
		if (community == null) {

			Logger.debug("The {} is not a valid Community.", body);
			OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.BAD_REQUEST, "bad_community",
					"The community is not right.");

		} else {

			try {

				community.validate("bad_community");
				this.repository.storeCommunity(community, stored -> {

					if (stored.failed()) {

						final Throwable cause = stored.cause();
						Logger.debug(cause, "Cannot store  {}.", community);
						OperationReponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

					} else {

						OperationReponseHandlers.responseOk(resultHandler, stored.result());
					}
				});

			} catch (final ValidationErrorException cause) {

				Logger.debug(cause, "The {} is not valid.", community);
				OperationReponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);
			}
		}

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void retrieveCommunity(String communityId, OperationRequest context,
			Handler<AsyncResult<OperationResponse>> resultHandler) {

		this.repository.searchCommunityObject(communityId, search -> {

			final JsonObject community = search.result();
			if (community == null) {

				Logger.debug(search.cause(), "Not found community for {}", communityId);
				OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.NOT_FOUND, "not_found_community",
						"Does not exist a community associated to '" + communityId + "'.");

			} else {

				OperationReponseHandlers.responseOk(resultHandler, community);

			}
		});

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void updateCommunity(String communityId, JsonObject body, OperationRequest context,
			Handler<AsyncResult<OperationResponse>> resultHandler) {

		final Community source = Model.fromJsonObject(body, Community.class);
		if (source == null) {

			Logger.debug("The {} is not a valid Community to update.", body);
			OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.BAD_REQUEST, "bad_community_to_update",
					"The community to update is not right.");

		} else {

			this.repository.searchCommunity(communityId, search -> {

				final Community target = search.result();
				if (target == null) {

					Logger.debug(search.cause(), "Not found community {} to update", communityId);
					OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.NOT_FOUND,
							"not_found_community_to_update",
							"You can not update the community '" + communityId + "', because it does not exist.");

				} else {

					try {

						final Community merged = target.merge(source, "bad_new_community");
						if (merged.equals(target)) {

							OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.BAD_REQUEST,
									"community_to_update_equal_to_original", "You can not update the community '" + communityId
											+ "', because the new values is equals to the current one.");

						} else {

							this.repository.updateCommunity(merged, update -> {

								if (update.failed()) {

									final Throwable cause = update.cause();
									Logger.debug(cause, "Cannot update  {}.", target);
									OperationReponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

								} else {

									final Community updated = update.result();
									OperationReponseHandlers.responseOk(resultHandler, updated);

								}

							});
						}

					} catch (final ValidationErrorException cause) {
						Logger.debug(cause, "Cannot update  {} with {}.", target, source);
						OperationReponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);
					}
				}
			});
		}

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void deleteCommunity(String communityId, OperationRequest context,
			Handler<AsyncResult<OperationResponse>> resultHandler) {

		this.repository.deleteCommunity(communityId, delete -> {

			if (delete.failed()) {

				final Throwable cause = delete.cause();
				Logger.debug(cause, "Cannot delete the community  {}.", communityId);
				OperationReponseHandlers.responseFailedWith(resultHandler, Status.NOT_FOUND, cause);

			} else {

				OperationReponseHandlers.responseOk(resultHandler);
			}

		});

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void retrieveCommunitiesPage(OperationRequest context, Handler<AsyncResult<OperationResponse>> resultHandler) {

		final JsonObject params = context.getParams().getJsonObject("query", new JsonObject());

		OperationReponseHandlers.responseOk(resultHandler, new CommunitiesPage());

	}

}
