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

package eu.internetofus.wenet_interaction_protocol_engine;

import eu.internetofus.common.components.interaction_protocol_engine.ProtocolMessage;
import eu.internetofus.common.components.models.CommunityProfile;
import eu.internetofus.common.components.models.Incentive;
import eu.internetofus.common.components.models.Task;
import eu.internetofus.common.components.models.TaskTransaction;
import eu.internetofus.common.components.models.TaskType;
import eu.internetofus.common.components.models.WeNetUserProfile;
import eu.internetofus.common.components.profile_manager.WeNetProfileManager;
import eu.internetofus.common.components.service.App;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import eu.internetofus.common.model.Model;
import eu.internetofus.common.model.ReflectionModel;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import javax.validation.constraints.NotNull;
import org.tinylog.Logger;

/**
 * Contains the data necessary for evaluate a protocol.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class ProtocolData extends ReflectionModel implements Model {

  /**
   * The task associated to the protocol.
   */
  public Task task;

  /**
   * The task type associated to the protocol.
   */
  public TaskType taskType;

  /**
   * The community to the protocol.
   */
  public CommunityProfile community;

  /**
   * The community to the protocol.
   */
  public WeNetUserProfile profile;

  /**
   * Load the protocol for the task transaction.
   *
   * @param transaction to extract the data.
   * @param vertx       event bus to use.
   *
   * @return the future protocol data.
   */
  public static Future<ProtocolData> createWith(@NotNull final TaskTransaction transaction, final Vertx vertx) {

    return loadTaskIn(transaction.taskId, new ProtocolData(), vertx)
        .compose(protocol -> loadTaskTypeIn(protocol, vertx)).compose(protocol -> loadCommunityIn(protocol, vertx))
        .compose(protocol -> loadProfileIn(transaction.actioneerId, protocol, vertx));

  }

  /**
   * Load the protocol for the task.
   *
   * @param task  to extract the data.
   * @param vertx event bus to use.
   *
   * @return the future protocol data.
   */
  public static Future<ProtocolData> createWith(@NotNull final Task task, final Vertx vertx) {

    final var data = new ProtocolData();
    data.task = task;
    return loadTaskTypeIn(task.taskTypeId, data, vertx)
        .compose(protocol -> loadCommunityIn(task.communityId, protocol, vertx))
        .compose(protocol -> loadProfileIn(task.requesterId, protocol, vertx));

  }

  /**
   * Load the protocol for the incentive.
   *
   * @param incentive to extract the data.
   * @param vertx     event bus to use.
   *
   * @return the future protocol data.
   */
  public static Future<ProtocolData> createWith(@NotNull final Incentive incentive, final Vertx vertx) {

    if (incentive.AppID == null) {

      return loadProfileIn(incentive.UserId, new ProtocolData(), vertx);

    } else {

      final Promise<ProtocolData> promise = Promise.promise();
      App.getOrCreateDefaultCommunityFor(incentive.AppID, vertx).onComplete(retrieve -> {

        final var protocolData = new ProtocolData();
        protocolData.community = retrieve.result();
        if (protocolData.community == null) {

          Logger.warn(retrieve.cause(), "Cannot found community for {}", incentive);
        }

        loadProfileIn(incentive.UserId, protocolData, vertx).onComplete(promise);

      });

      return promise.future();

    }

  }

  /**
   * Load the data of a task into a protocol.
   *
   * @param taskId       identifier of the task.
   * @param protocolData data to fill in.
   * @param vertx        event bus to use.
   *
   * @return the future protocol data with the task.
   */
  public static Future<ProtocolData> loadTaskIn(final String taskId, final ProtocolData protocolData,
      final Vertx vertx) {

    if (taskId == null) {

      return Future.succeededFuture(protocolData);

    } else {

      final Promise<ProtocolData> promise = Promise.promise();
      if (protocolData.task != null && !taskId.equals(protocolData.task.id)) {

        protocolData.task = null;
      }
      if (protocolData.task != null) {

        promise.complete(protocolData);

      } else {

        WeNetTaskManager.createProxy(vertx).retrieveTask(taskId).onComplete(retrieve -> {

          final var task = retrieve.result();
          if (task != null) {

            protocolData.task = task;

          } else {

            Logger.warn(retrieve.cause(), "Not found task {}", taskId);
          }
          promise.complete(protocolData);

        });
      }
      return promise.future();
    }
  }

  /**
   * Load the data of a task type into a protocol.
   *
   * @param protocolData data to fill in.
   * @param vertx        event bus to use.
   *
   * @return the future protocol data with the task type.
   */
  public static Future<ProtocolData> loadTaskTypeIn(final ProtocolData protocolData, final Vertx vertx) {

    String taskTypeId = null;
    if (protocolData.task != null) {

      taskTypeId = protocolData.task.taskTypeId;
    }
    return loadTaskTypeIn(taskTypeId, protocolData, vertx);
  }

  /**
   * Load the data of a task type into a protocol.
   *
   * @param taskTypeId   identifier of the task type.
   * @param protocolData data to fill in.
   * @param vertx        event bus to use.
   *
   * @return the future protocol data with the task type.
   */
  public static Future<ProtocolData> loadTaskTypeIn(final String taskTypeId, final ProtocolData protocolData,
      final Vertx vertx) {

    if (taskTypeId == null) {

      return Future.succeededFuture(protocolData);

    } else {

      if (protocolData.taskType != null && !taskTypeId.equals(protocolData.taskType.id)) {

        protocolData.taskType = null;
      }

      final Promise<ProtocolData> promise = Promise.promise();
      if (protocolData.taskType != null) {

        promise.complete(protocolData);

      } else {

        WeNetTaskManager.createProxy(vertx).retrieveTaskType(taskTypeId).onComplete(retrieve -> {

          final var tasktype = retrieve.result();
          if (tasktype != null) {

            protocolData.taskType = tasktype;

          } else {

            Logger.warn(retrieve.cause(), "Not found task type {}", taskTypeId);

          }
          promise.complete(protocolData);
        });

      }

      return promise.future();
    }
  }

  /**
   * Load the data of a task type into a protocol.
   *
   * @param protocolData data to fill in.
   * @param vertx        event bus to use.
   *
   * @return the future protocol data with the task type.
   */
  public static Future<ProtocolData> loadCommunityIn(final ProtocolData protocolData, final Vertx vertx) {

    String communityId = null;
    if (protocolData.task != null) {

      communityId = protocolData.task.communityId;
    }
    return loadCommunityIn(communityId, protocolData, vertx);
  }

  /**
   * Load the data of a task type into a protocol.
   *
   * @param communityId  identifier of the task type.
   * @param protocolData data to fill in.
   * @param vertx        event bus to use.
   *
   * @return the future protocol data with the task type.
   */
  public static Future<ProtocolData> loadCommunityIn(final String communityId, final ProtocolData protocolData,
      final Vertx vertx) {

    if (communityId == null) {

      return Future.succeededFuture(protocolData);

    } else {

      if (protocolData.community != null && !communityId.equals(protocolData.community.id)) {

        protocolData.community = null;
      }

      final Promise<ProtocolData> promise = Promise.promise();
      if (protocolData.community != null) {

        promise.complete(protocolData);

      } else {

        WeNetProfileManager.createProxy(vertx).retrieveCommunity(communityId).onComplete(retrieve -> {

          final var community = retrieve.result();
          if (community != null) {

            protocolData.community = community;

          } else {

            Logger.warn(retrieve.cause(), "Not found community {}", communityId);

          }
          promise.complete(protocolData);
        });

      }

      return promise.future();
    }
  }

  /**
   * Load the data of a task type into a protocol.
   *
   * @param profileId    identifier of the task type.
   * @param protocolData data to fill in.
   * @param vertx        event bus to use.
   *
   * @return the future protocol data with the task type.
   */
  public static Future<ProtocolData> loadProfileIn(final String profileId, final ProtocolData protocolData,
      final Vertx vertx) {

    if (profileId == null) {

      return Future.succeededFuture(protocolData);

    } else {

      if (protocolData.profile != null && !profileId.equals(protocolData.profile.id)) {

        protocolData.profile = null;
      }

      final Promise<ProtocolData> promise = Promise.promise();
      if (protocolData.profile != null) {

        promise.complete(protocolData);

      } else {

        WeNetProfileManager.createProxy(vertx).retrieveProfile(profileId).onComplete(retrieve -> {

          final var profile = retrieve.result();
          if (profile != null) {

            protocolData.profile = profile;

          } else {

            Logger.warn(retrieve.cause(), "Not found profile {}", profileId);

          }
          promise.complete(protocolData);
        });

      }

      return promise.future();
    }
  }

  /**
   * Check if the protocol has some protocol norms to validate.
   *
   * @return {@code true} if the data defined has any protocol norm.
   */
  public boolean hasProtocolNorms() {

    return this.community != null && this.community.norms != null && !this.community.norms.isEmpty()
        || this.taskType != null && this.taskType.norms != null && !this.taskType.norms.isEmpty();

  }

  /**
   * Create the protocol for the message.
   *
   * @param message to obtain the data.
   * @param vertx   event bus to use.
   *
   * @return the future protocol data.
   */
  public static Future<ProtocolData> createWith(@NotNull final ProtocolMessage message, final Vertx vertx) {

    final var data = new ProtocolData();
    return loadTaskIn(message.taskId, data, vertx).compose(protocol -> loadTaskTypeIn(protocol, vertx))
        .compose(protocol -> loadCommunityIn(message.communityId, protocol, vertx)).compose(protocol -> {

          if (message.receiver != null) {

            return loadProfileIn(message.receiver.userId, protocol, vertx);

          } else {

            return Future.succeededFuture(protocol);
          }

        });

  }

}
