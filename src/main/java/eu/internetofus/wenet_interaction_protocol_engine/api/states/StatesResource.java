/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */
package eu.internetofus.wenet_interaction_protocol_engine.api.states;

import eu.internetofus.common.components.WeNetModelContext;
import eu.internetofus.common.components.interaction_protocol_engine.State;
import eu.internetofus.common.model.Merges;
import eu.internetofus.common.model.TimeManager;
import eu.internetofus.common.vertx.ModelResources;
import eu.internetofus.common.vertx.ServiceContext;
import eu.internetofus.common.vertx.ServiceResponseHandlers;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.StatesRepository;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;

/**
 * Implementation of the {@link States}
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class StatesResource implements States {

  /**
   * The event bus that is using.
   */
  protected Vertx vertx;

  /**
   * Create a new resource.
   *
   * @param vertx event bus to use.
   */
  public StatesResource(final Vertx vertx) {

    this.vertx = vertx;

  }

  /**
   * Retrieve an state.
   *
   * @param communityId   identifier of the community.
   * @param taskId        identifier of the task.
   * @param userId        identifier of the user.
   * @param resultHandler to notify of the result.
   */
  protected void retrieveState(final String communityId, final String taskId, final String userId,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    StatesRepository.createProxy(this.vertx).searchState(communityId, taskId, userId).onComplete(search -> {

      var state = search.result();
      if (state == null) {

        state = new State();
        state.communityId = communityId;
        state.taskId = taskId;
        state.userId = userId;
      }

      ServiceResponseHandlers.responseOk(resultHandler, state);
    });

  }

  /**
   * Merge an state.
   *
   * @param communityId   identifier of the community.
   * @param taskId        identifier of the task.
   * @param userId        identifier of the user.
   * @param body          that has received.
   * @param request       that has been received.
   * @param resultHandler to notify of the result.
   */
  protected void mergeState(final String communityId, final String taskId, final String userId, final JsonObject body,
      final ServiceRequest request, final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var model = WeNetModelContext.creteWeNetContext("state", State.class, this.vertx);
    final var context = new ServiceContext(request, resultHandler);
    ModelResources.toModel(body, model, context, () -> {

      StatesRepository.createProxy(this.vertx).searchState(communityId, taskId, userId).onComplete(search -> {

        final var state = search.result();
        if (state == null) {

          model.source.communityId = communityId;
          model.source.taskId = taskId;
          model.source.userId = userId;
          model.source._creationTs = model.source._lastUpdateTs = TimeManager.now();
          StatesRepository.createProxy(this.vertx).storeState(model.source).onComplete(stored -> {

            ServiceResponseHandlers.responseOk(resultHandler, stored.result());

          });

        } else {

          state.attributes = Merges.mergeJsonObjects(state.attributes, model.source.attributes);
          state._lastUpdateTs = TimeManager.now();

          StatesRepository.createProxy(this.vertx).updateState(state).onComplete(updated -> {

            ServiceResponseHandlers.responseOk(resultHandler, state);

          });
        }

      });

    });
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrieveCommunityUserState(final String communityId, final String userId, final ServiceRequest context,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    this.retrieveState(communityId, null, userId, resultHandler);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void mergeCommunityUserState(final String communityId, final String userId, final JsonObject body,
      final ServiceRequest request, final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    this.mergeState(communityId, null, userId, body, request, resultHandler);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrieveTaskUserState(final String taskId, final String userId, final ServiceRequest context,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    this.retrieveState(null, taskId, userId, resultHandler);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void mergeTaskUserState(final String taskId, final String userId, final JsonObject body,
      final ServiceRequest request, final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    this.mergeState(null, taskId, userId, body, request, resultHandler);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrieveUserState(final String userId, final ServiceRequest context,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    this.retrieveState(null, null, userId, resultHandler);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void mergeUserState(final String userId, final JsonObject body, final ServiceRequest request,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    this.mergeState(null, null, userId, body, request, resultHandler);

  }

}
